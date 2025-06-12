ADD_EXECUTABLE(dots_convenience_test convenience_test.cpp)

TARGET_LINK_LIBRARIES(dots_convenience_test PRIVATE
  dots_cpp
  Boost::unit_test_framework
)

ADD_TEST(NAME dots_convenience_test COMMAND dots_convenience_test)

SET_SAFIR_TEST_PROPERTIES(TEST dots_convenience_test)
