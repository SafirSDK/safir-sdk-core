cmake_minimum_required(VERSION 2.8)

project(lluf_crash_reporter CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

INCLUDE_DIRECTORIES(. google-breakpad/src/src)
add_subdirectory(google-breakpad)

add_library(lluf_crash_reporter SHARED CrashReporter.cpp Safir/Utilities/CrashReporter.h)

FIND_PACKAGE(Boost COMPONENTS filesystem thread system)

TARGET_LINK_LIBRARIES(lluf_crash_reporter "${CMAKE_CURRENT_BINARY_DIR}/google-breakpad/bin/src/client/linux/libbreakpad_client.a")

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(lluf_crash_reporter ${Boost_LIBRARIES})
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
