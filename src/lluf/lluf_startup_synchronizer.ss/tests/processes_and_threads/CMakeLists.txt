ADD_EXECUTABLE(ss_processes_and_threads_test ss_processes_and_threads_test.cpp)

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES(ss_processes_and_threads_test lluf_startup_synchronizer ${Boost_LIBRARIES})
ENDIF()

ADD_TEST(StartupSynchronizer_processes_and_threads ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
SET_TESTS_PROPERTIES(StartupSynchronizer_processes_and_threads PROPERTIES TIMEOUT 120)
SET_TESTS_PROPERTIES(StartupSynchronizer_processes_and_threads PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")
