ADD_EXECUTABLE(crasher crasher.cpp)

TARGET_LINK_LIBRARIES (crasher lluf_crash_reporter)

ADD_TEST(CrashReporter_simple ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(TEST CrashReporter_simple)
