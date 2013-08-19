ADD_EXECUTABLE(sleeper sleeper.cpp)

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES (sleeper lluf_crash_reporter ${Boost_LIBRARIES})
ENDIF()

ADD_TEST(CrashReporter_external_signals ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_TESTS_PROPERTIES(CrashReporter_external_signals PROPERTIES TIMEOUT 300)
SET_TESTS_PROPERTIES(CrashReporter_external_signals PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")
