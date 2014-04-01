#TODO
if (NOT "${CMAKE_CONFIGURATION_TYPES}" STREQUAL "")
  message(FATAL_ERROR "This project does not support being built in visual studio. Please use NMake on windows and make on linux")
endif()

#TODO
#this library has an interface which is okay to call between debug/release runtimes,
#and we only want one version of it, so we don't want the "d" suffix on debug builds on windows.
#Must be set before including safir_sdk_core_config.cmake
SET(NO_LIBRARY_POSTFIXES 1) 

add_subdirectory(google-breakpad)

add_library(lluf_crash_reporter SHARED CrashReporter.cpp Safir/Utilities/CrashReporter.h)

TARGET_INCLUDE_DIRECTORIES(lluf_crash_reporter PUBLIC . PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/google-breakpad/src/src)

FIND_PACKAGE(Boost COMPONENTS filesystem thread system)

TARGET_LINK_LIBRARIES(lluf_crash_reporter lluf_config ${Boost_LIBRARIES})

if (MSVC)
  TARGET_LINK_LIBRARIES(lluf_crash_reporter 
  "${CMAKE_CURRENT_BINARY_DIR}/google-breakpad/src/breakpad_client.lib")
else()
  #TODO: set only for this target (?!)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-local-typedefs")
  TARGET_LINK_LIBRARIES(lluf_crash_reporter 
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
