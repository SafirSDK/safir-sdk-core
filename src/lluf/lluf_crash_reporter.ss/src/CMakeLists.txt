find_package(sentry-breakpad)

add_library(lluf_crash_reporter SHARED CrashReporter.cpp)
safir_mark_single_abi(lluf_crash_reporter)

TARGET_LINK_LIBRARIES(lluf_crash_reporter PRIVATE
  lluf_config
  sentry-breakpad::sentry-breakpad
  Boost::filesystem
  Boost::thread
  )

SAFIR_INSTALL(TARGETS lluf_crash_reporter)
