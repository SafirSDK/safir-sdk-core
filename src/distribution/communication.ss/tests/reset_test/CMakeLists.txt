add_definitions(-DSAFIR_TEST)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(communication_reset_test ${sources} ${headers})

TARGET_LINK_LIBRARIES(communication_reset_test PRIVATE
  communication
  lluf_internal
  Boost::unit_test_framework
  Boost::thread)

ADD_TEST(NAME Communication_ResetTest COMMAND communication_reset_test)
SET_SAFIR_TEST_PROPERTIES(TEST Communication_ResetTest TIMEOUT 180)
