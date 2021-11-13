add_definitions(-DSAFIR_TEST)

ADD_EXECUTABLE(Distribution_test Distribution_test.cpp ../../src/Distribution.h)
ADD_EXECUTABLE(PoolHandler_test PoolHandlerTest.cpp
                ../../src/PoolDistributionRequestSender.h
                ../../src/PoolDistributionHandler.h)

TARGET_LINK_LIBRARIES(Distribution_test PRIVATE
  lluf_utils
  communication
  system_picture
  control_utils
  Boost::system
  Boost::unit_test_framework)

TARGET_LINK_LIBRARIES(PoolHandler_test PRIVATE
  communication
  Boost::atomic
  Boost::system
  Boost::thread
  Boost::unit_test_framework)


ADD_TEST(NAME Distribution_test COMMAND Distribution_test)
SET_SAFIR_TEST_PROPERTIES(TEST Distribution_test TIMEOUT 600)

ADD_TEST(NAME PoolHandler_test COMMAND PoolHandler_test)
SET_SAFIR_TEST_PROPERTIES(TEST PoolHandler_test TIMEOUT 600)

