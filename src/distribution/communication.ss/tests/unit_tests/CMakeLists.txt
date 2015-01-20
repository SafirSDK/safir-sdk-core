add_definitions(-DSAFIR_TEST)

file(GLOB headers *.h)
file(GLOB sources *.cpp)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})
PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS ../../src/CommunicationMessage.proto)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${PROTO_SRCS} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267 /wd4018")
endif()

add_executable(unit_tests ${sources} ${headers} ${PROTO_SRCS} ${PROTO_HDRS} ../../src/MessageQueue.h
                ../../src/DeliveryHandler.h ../../src/DataSender.h
                ../../src/Node.h ../../src/NodeType.h
                ../../src/Writer.h ../../src/HeartbeatSender.h
                ../../src/DataReceiver.h ../../src/Discoverer.h ../../src/Utilities.h)

target_link_libraries(unit_tests PRIVATE
  lluf_utils
  lluf_id
  ${PROTOBUF_LITE_LIBRARIES})

ADD_TEST(NAME MessageQueueTest COMMAND unit_tests MessageQueueTest)
SET_SAFIR_TEST_PROPERTIES(TEST MessageQueueTest TIMEOUT 360)

ADD_TEST(NAME HeartbeatSenderTest COMMAND unit_tests HeartbeatSenderTest)
SET_SAFIR_TEST_PROPERTIES(TEST HeartbeatSenderTest TIMEOUT 360)

ADD_TEST(NAME DataReceiverTest COMMAND unit_tests DataReceiverTest)
SET_SAFIR_TEST_PROPERTIES(TEST DataReceiverTest TIMEOUT 360)

ADD_TEST(NAME DataSenderTest COMMAND unit_tests DataSenderTest)
SET_SAFIR_TEST_PROPERTIES(TEST DataSenderTest TIMEOUT 360)

ADD_TEST(NAME DeliveryHandlerTest COMMAND unit_tests DeliveryHandlerTest)
SET_SAFIR_TEST_PROPERTIES(TEST DeliveryHandlerTest TIMEOUT 360)

ADD_TEST(NAME DiscovererTest COMMAND unit_tests DiscovererTest)
SET_SAFIR_TEST_PROPERTIES(TEST DiscovererTest TIMEOUT 360)
