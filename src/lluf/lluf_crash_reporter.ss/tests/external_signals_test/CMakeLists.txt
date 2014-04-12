ADD_EXECUTABLE(sleeper sleeper.cpp)

TARGET_LINK_LIBRARIES (sleeper PRIVATE
  lluf_crash_reporter
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

ADD_TEST(CrashReporter_external_signals ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(TEST CrashReporter_external_signals TIMEOUT 300)
