ADD_EXECUTABLE(crash_reporter_sleeper sleeper.cpp)

TARGET_LINK_LIBRARIES (crash_reporter_sleeper PRIVATE
  lluf_crash_reporter
  Boost::thread
)

ADD_TEST(NAME CrashReporter_external_signals COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --sleeper-exe $<TARGET_FILE:crash_reporter_sleeper>)

SET_SAFIR_TEST_PROPERTIES(TEST CrashReporter_external_signals TIMEOUT 300)
