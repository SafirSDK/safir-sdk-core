ADD_DEFINITIONS(-Ddose_internal_EXPORTS)
ADD_EXECUTABLE(lamport_clock_test lamport_clock_test.cpp ../../src/LamportClocks.cpp)
TARGET_INCLUDE_DIRECTORIES(lamport_clock_test PRIVATE ../../src/include)

TARGET_LINK_LIBRARIES(lamport_clock_test PRIVATE
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})

ADD_TEST(NAME LamportClocks COMMAND lamport_clock_test)

SET_SAFIR_TEST_PROPERTIES(TEST LamportClocks TIMEOUT 2400)
