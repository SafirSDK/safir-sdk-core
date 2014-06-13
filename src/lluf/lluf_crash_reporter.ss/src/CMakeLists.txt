#this library has an interface which is okay to call between debug/release runtimes,
#and we only want one version of it, so we never want the "d" suffix on debug builds,
#which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

add_subdirectory(google-breakpad)


if (MSVC)
  LINK_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR}/google-breakpad/src/)
else()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-local-typedefs")
  LINK_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR}/google-breakpad/src/src/client/linux)
endif()

add_library(lluf_crash_reporter SHARED CrashReporter.cpp)

TARGET_INCLUDE_DIRECTORIES(lluf_crash_reporter PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/google-breakpad/src/src)

TARGET_LINK_LIBRARIES(lluf_crash_reporter PRIVATE 
  lluf_config 
  breakpad_client
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

add_dependencies(lluf_crash_reporter google-breakpad)

SAFIR_INSTALL(TARGETS lluf_crash_reporter)
