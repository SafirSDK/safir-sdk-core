FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(SystemLog_test ${sources})

TARGET_LINK_LIBRARIES(SystemLog_test PRIVATE lluf_utils)

ADD_TEST(SystemLog ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py ${CMAKE_CURRENT_SOURCE_DIR}/testconf)
