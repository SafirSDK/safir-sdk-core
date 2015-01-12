ADD_EXECUTABLE(dumper dumper.cpp)

TARGET_LINK_LIBRARIES (dumper lluf_crash_reporter)

ADD_TEST(NAME CrashReporter_immediate_dump COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(TEST CrashReporter_immediate_dump)
