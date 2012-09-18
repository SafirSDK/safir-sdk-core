ADD_EXECUTABLE(sleeper sleeper.cpp)

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES (sleeper lluf_crash_reporter ${Boost_LIBRARIES})
ELSE (NOT MSVC)
  TARGET_LINK_LIBRARIES (sleeper lluf_crash_reporter)
ENDIF()

ADD_TEST(CrashReporter_external_signals ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

