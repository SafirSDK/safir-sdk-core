find_package(PythonInterp)

ADD_TEST(crash_dump_monitor_test ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py ${CMAKE_CURRENT_SOURCE_DIR}/testconf)

SET_SAFIR_TEST_PROPERTIES(crash_dump_monitor_test 120)




