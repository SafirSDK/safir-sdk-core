ADD_EXECUTABLE(AsioStrandWrap_test test.cpp)

TARGET_LINK_LIBRARIES(AsioStrandWrap_test PRIVATE
  Boost::thread)

ADD_TEST(NAME AsioStrandWrap COMMAND AsioStrandWrap_test)
SET_SAFIR_TEST_PROPERTIES(TEST AsioStrandWrap)
