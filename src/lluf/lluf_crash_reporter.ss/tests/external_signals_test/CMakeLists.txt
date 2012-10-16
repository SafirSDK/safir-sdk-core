ADD_EXECUTABLE(sleeper sleeper.cpp)

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES (sleeper lluf_crash_reporter ${Boost_LIBRARIES})
ENDIF()

ADD_TEST(CrashReporter_external_signals ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

set_tests_properties(CrashReporter_external_signals PROPERTIES TIMEOUT 300)
