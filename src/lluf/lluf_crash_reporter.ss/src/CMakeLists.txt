#TODO
if (NOT "${CMAKE_CONFIGURATION_TYPES}" STREQUAL "")
  message(FATAL_ERROR "This project does not support being built in visual studio. Please use NMake on windows and make on linux")
endif()

#this library has an interface which is okay to call between debug/release runtimes,
#and we only want one version of it, so we never want the "d" suffix on debug builds,
#which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

add_subdirectory(google-breakpad)

add_library(lluf_crash_reporter SHARED CrashReporter.cpp Safir/Utilities/CrashReporter.h)

TARGET_INCLUDE_DIRECTORIES(lluf_crash_reporter PUBLIC . PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/google-breakpad/src/src)

TARGET_LINK_LIBRARIES(lluf_crash_reporter PRIVATE 
  lluf_config 
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

if (MSVC)
  TARGET_LINK_LIBRARIES(lluf_crash_reporter PRIVATE 
  "${CMAKE_CURRENT_BINARY_DIR}/google-breakpad/src/breakpad_client.lib")
else()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-local-typedefs")
  TARGET_LINK_LIBRARIES(lluf_crash_reporter PRIVATE 
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
