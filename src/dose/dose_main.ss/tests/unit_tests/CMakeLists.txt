ADD_EXECUTABLE(timer_test timer_test.cpp ../../src/dose_main_timers.h ../../src/dose_main_timers.cpp)

TARGET_LINK_LIBRARIES(timer_test PRIVATE
  dose_internal
  lluf_utils)

ADD_TEST(NAME dose_main_timers COMMAND timer_test)

SET_SAFIR_TEST_PROPERTIES(TEST dose_main_timers TIMEOUT 600)
