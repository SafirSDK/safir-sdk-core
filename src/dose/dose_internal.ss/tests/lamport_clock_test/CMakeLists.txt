ADD_EXECUTABLE(lamport_clock_test lamport_clock_test.cpp)

TARGET_LINK_LIBRARIES(lamport_clock_test PRIVATE
  dose_internal
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})

ADD_TEST(NAME LamportClocks COMMAND lamport_clock_test)

SET_SAFIR_TEST_PROPERTIES(TEST LamportClocks TIMEOUT 2400)
