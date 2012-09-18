ADD_EXECUTABLE(crasher crasher.cpp)

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES (crasher lluf_crash_reporter ${Boost_LIBRARIES})
ELSE (NOT MSVC)
  TARGET_LINK_LIBRARIES (crasher lluf_crash_reporter)
ENDIF()

ADD_TEST(CrashReporter_simple ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
#ADD_TEST(CrashReporter_simple crasher)
