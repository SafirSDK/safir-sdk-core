FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(communication_reset_test ${sources} ${headers})

TARGET_LINK_LIBRARIES(communication_reset_test PRIVATE
  communication
  lluf_internal
  Boost::unit_test_framework
  Boost::thread)

ADD_TEST(NAME CommunicationResetTest COMMAND communication_reset_test)
SET_SAFIR_TEST_PROPERTIES(TEST CommunicationResetTest TIMEOUT 180)
