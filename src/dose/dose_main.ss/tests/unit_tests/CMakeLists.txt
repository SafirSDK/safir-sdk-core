add_definitions(-DSAFIR_TEST)

ADD_EXECUTABLE(timer_test timer_test.cpp ../../src/dose_main_timers.h ../../src/dose_main_timers.cpp)
ADD_EXECUTABLE(Distribution_test Distribution_test.cpp ../../src/Distribution.h)
ADD_EXECUTABLE(PoolHandler_test PoolHandlerTest.cpp ../../src/PoolDistribution.h)

TARGET_LINK_LIBRARIES(timer_test PRIVATE
  dose_internal
  lluf_utils)

TARGET_LINK_LIBRARIES(Distribution_test PRIVATE
  communication
  system_picture
  control_utils
  ${Boost_SYSTEM_LIBRARY}
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})

TARGET_LINK_LIBRARIES(PoolHandler_test PRIVATE
  communication
  ${Boost_SYSTEM_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})


ADD_TEST(NAME dose_main_timers COMMAND timer_test)
SET_SAFIR_TEST_PROPERTIES(TEST dose_main_timers TIMEOUT 600)

ADD_TEST(NAME Distribution_test COMMAND Distribution_test)
SET_SAFIR_TEST_PROPERTIES(TEST Distribution_test TIMEOUT 600)

ADD_TEST(NAME PoolHandler_test COMMAND PoolHandler_test)
SET_SAFIR_TEST_PROPERTIES(TEST PoolHandler_test TIMEOUT 600)

