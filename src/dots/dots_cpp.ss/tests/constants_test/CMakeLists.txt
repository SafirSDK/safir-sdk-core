ADD_EXECUTABLE(constants_test constants_test.cpp)

TARGET_LINK_LIBRARIES(constants_test PRIVATE
  dots_cpp)

ADD_TEST(NAME constants_test COMMAND constants_test)

SET_SAFIR_TEST_PROPERTIES(TEST constants_test)
