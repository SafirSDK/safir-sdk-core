ADD_EXECUTABLE(dumper dumper.cpp)

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES (dumper lluf_crash_reporter ${Boost_LIBRARIES})
ENDIF()

ADD_TEST(CrashReporter_immediate_dump ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(TEST CrashReporter_immediate_dump)
