#this library has an interface which is okay to call between debug/release runtimes,
#and we only want one version of it, so we never want the "d" suffix on debug builds,
#which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")


FIND_PATH(BREAKPAD_INCLUDE_DIRECTORY
  NAMES
    client/linux/handler/exception_handler.h
  PATHS
    /usr/include/breakpad
    /usr/local/include/breakpad)

FIND_LIBRARY(BREAKPAD_LIBRARY
  NAMES
    breakpad_client
  PATHS
    /usr/lib
    /usr/local/lib)

if (BREAKPAD_LIBRARY AND BREAKPAD_INCLUDE_DIRECTORY)
  MESSAGE(STATUS "Google breakpad seems to be installed on this computer, will use the installed version.")
  set(BREAKPAD_FOUND True)
else()
  MESSAGE(STATUS "Google breakpad is not installed on this computer, will use our included version.")
  set(BREAKPAD_FOUND False)
endif()

#breakpad needs the GNU extensions to build, so we append a new -std= here
#also disable some warnings
if (NOT MSVC)
  string (REPLACE "-Wpedantic" "" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=gnu++11")
endif()

#we add our own breakpad build, which which should already be downloaded (it's a git submodule)
if (NOT BREAKPAD_FOUND)
  add_subdirectory(google-breakpad)

  set(BREAKPAD_INCLUDE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/google-breakpad/breakpad/src)

  set(BREAKPAD_LIBRARY breakpad_client)

  if (MSVC)
    link_directories(${CMAKE_CURRENT_BINARY_DIR}/google-breakpad/bin/)
  else()
    link_directories(${CMAKE_CURRENT_SOURCE_DIR}/google-breakpad/breakpad/src/client/linux/)
  endif()
endif()

add_library(lluf_crash_reporter SHARED CrashReporter.cpp)

TARGET_INCLUDE_DIRECTORIES(lluf_crash_reporter PRIVATE ${BREAKPAD_INCLUDE_DIRECTORY})

TARGET_LINK_LIBRARIES(lluf_crash_reporter PRIVATE
  lluf_config
  ${BREAKPAD_LIBRARY}
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

if (NOT BREAKPAD_FOUND)
  add_dependencies(lluf_crash_reporter google-breakpad)
endif()

SAFIR_INSTALL(TARGETS lluf_crash_reporter)
