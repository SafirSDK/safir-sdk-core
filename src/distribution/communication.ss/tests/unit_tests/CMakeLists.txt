add_definitions(-DSAFIR_TEST)

file(GLOB headers *.h)
file(GLOB sources *.cpp)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})

add_executable(communication_unit_tests ${sources} ${headers} ../../src/MessageQueue.h
                ../../src/DeliveryHandler.h ../../src/DataSender.h
                ../../src/Node.h ../../src/NodeType.h
                ../../src/Writer.h ../../src/HeartbeatSender.h
                ../../src/DataReceiver.h ../../src/Discoverer.h ../../src/Utilities.h
                ../../src/CommunicationMessage.proto)

PROTOBUF_GENERATE(TARGET communication_unit_tests APPEND_PATH OUT_VAR generated_files)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${generated_files} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267 /wd4100 /wd4702")
endif()


target_link_libraries(communication_unit_tests PRIVATE
  lluf_internal
  lluf_id
  protobuf::protobuf
  Boost::regex)

ADD_TEST(NAME Communication_MessageQueueTest COMMAND communication_unit_tests MessageQueueTest)
SET_SAFIR_TEST_PROPERTIES(TEST Communication_MessageQueueTest TIMEOUT 360)

ADD_TEST(NAME Communication_HeartbeatSenderTest COMMAND communication_unit_tests HeartbeatSenderTest)
SET_SAFIR_TEST_PROPERTIES(TEST Communication_HeartbeatSenderTest TIMEOUT 360)

ADD_TEST(NAME Communication_DataReceiverTest COMMAND communication_unit_tests DataReceiverTest)
SET_SAFIR_TEST_PROPERTIES(TEST Communication_DataReceiverTest TIMEOUT 360)

ADD_TEST(NAME Communication_DataSenderTest COMMAND communication_unit_tests DataSenderTest)
SET_SAFIR_TEST_PROPERTIES(TEST Communication_DataSenderTest TIMEOUT 360)

ADD_TEST(NAME Communication_DeliveryHandlerTest COMMAND communication_unit_tests DeliveryHandlerTest)
SET_SAFIR_TEST_PROPERTIES(TEST Communication_DeliveryHandlerTest TIMEOUT 360)

ADD_TEST(NAME Communication_DiscovererTest COMMAND communication_unit_tests DiscovererTest)
SET_SAFIR_TEST_PROPERTIES(TEST Communication_DiscovererTest TIMEOUT 360)

ADD_TEST(NAME Communication_AllocatorTest COMMAND communication_unit_tests AllocatorTest)
SET_SAFIR_TEST_PROPERTIES(TEST Communication_AllocatorTest TIMEOUT 360)

ADD_TEST(NAME Communication_ResolverTest COMMAND communication_unit_tests ResolverTest)
SET_SAFIR_TEST_PROPERTIES(TEST Communication_ResolverTest TIMEOUT 360)
