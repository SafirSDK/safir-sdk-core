FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(SystemLog_test ${sources})

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES(SystemLog_test lluf_utils ${Boost_LIBRARIES})
ENDIF ()

ADD_TEST(SystemLog ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
