cmake_minimum_required(VERSION 2.8)

project(crash_dump_monitor_test CXX)

include($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

#find_package(Boost COMPONENTS thread date_time system)

find_package(PythonInterp)

enable_testing()

ADD_TEST(crash_dump_monitor_test ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py ${CMAKE_CURRENT_SOURCE_DIR}/testconf)

SET_SAFIR_TEST_PROPERTIES(crash_dump_monitor_test 120)




