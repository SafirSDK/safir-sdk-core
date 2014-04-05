ADD_EXECUTABLE(constants_test constants_test.cpp)

TARGET_LINK_LIBRARIES(constants_test dots_cpp)

ADD_TEST(constants_test constants_test)

SET_SAFIR_TEST_PROPERTIES(constants_test)
