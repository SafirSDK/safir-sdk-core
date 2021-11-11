#this library has an interface which is okay to call between debug/release runtimes,
#and we only want one version of it, so we never want the "d" suffix on debug builds,
#which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

find_package(sentry-breakpad)

add_library(lluf_crash_reporter SHARED CrashReporter.cpp)

TARGET_LINK_LIBRARIES(lluf_crash_reporter PRIVATE
  lluf_config
  sentry-breakpad::sentry-breakpad
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS lluf_crash_reporter)
