ADD_EXECUTABLE(dumper dumper.cpp)

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES (dumper lluf_crash_reporter ${Boost_LIBRARIES})
ENDIF()

ADD_TEST(CrashReporter_immediate_dump ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_TESTS_PROPERTIES(CrashReporter_immediate_dump PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")
