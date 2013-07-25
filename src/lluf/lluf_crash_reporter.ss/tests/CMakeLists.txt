cmake_minimum_required(VERSION 2.8)

project(lluf_crash_reporter_test CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FIND_PACKAGE(Boost COMPONENTS thread date_time system)

FIND_PACKAGE(PythonInterp)

ENABLE_TESTING()

ADD_EXECUTABLE(start_stop start_stop.cpp)
IF (NOT MSVC)
  TARGET_LINK_LIBRARIES (start_stop lluf_crash_reporter)
ENDIF()


FIND_SOURCE_ROOT(SOURCE_ROOT)
ADD_TEST(CrashReporter_start_stop start_stop)
SET_TESTS_PROPERTIES(CrashReporter_start_stop PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")

ADD_SUBDIRECTORY(simple_crash_test)
ADD_SUBDIRECTORY(external_signals_test)
ADD_SUBDIRECTORY(immediate_dump_test)


