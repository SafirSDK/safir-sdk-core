ADD_EXECUTABLE(dumper dumper.cpp)

TARGET_LINK_LIBRARIES (dumper PRIVATE lluf_crash_reporter)

ADD_TEST(NAME CrashReporter_immediate_dump COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --dumper-exe $<TARGET_FILE:dumper>)

SET_SAFIR_TEST_PROPERTIES(TEST CrashReporter_immediate_dump)
