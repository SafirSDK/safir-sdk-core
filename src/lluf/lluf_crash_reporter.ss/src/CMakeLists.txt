cmake_minimum_required(VERSION 2.8)

project(lluf_crash_reporter C CXX)

if (NOT "${CMAKE_CONFIGURATION_TYPES}" STREQUAL "")
  message(FATAL_ERROR "This project does not support being built in visual studio. Please use NMake on windows and make on linux")
endif()

#this library has an interface which is okay to call between debug/release runtimes,
#and we only want one version of it, so we don't want the "d" suffix on debug builds on windows.
#Must be set before including cmake_settings.txt
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE_DIRECTORIES(. google-breakpad/src/src)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

add_subdirectory(google-breakpad)

add_library(lluf_crash_reporter SHARED CrashReporter.cpp Safir/Utilities/CrashReporter.h)

FIND_PACKAGE(Boost COMPONENTS filesystem thread system)

if (MSVC)
  TARGET_LINK_LIBRARIES(lluf_crash_reporter 
  "${CMAKE_CURRENT_BINARY_DIR}/google-breakpad/src/breakpad_client.lib")
else()
  TARGET_LINK_LIBRARIES(lluf_crash_reporter 
    ${Boost_LIBRARIES}
    "${CMAKE_CURRENT_BINARY_DIR}/google-breakpad/src/src/client/linux/libbreakpad_client.a")
endif()

add_dependencies(lluf_crash_reporter google-breakpad)


INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS lluf_crash_reporter
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(lluf_crash_reporter)
