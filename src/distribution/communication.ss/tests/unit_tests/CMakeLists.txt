cmake_minimum_required(VERSION 2.8.5)
project(unit_tests CXX C) #C is needed by FindProtobuf

add_definitions(-DSAFIR_TEST)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

find_package(Boost COMPONENTS thread system chrono)
find_package(Protobuf REQUIRED)

file(GLOB headers *.h)
file(GLOB sources *.cpp)

INCLUDE_DIRECTORIES(${PROTOBUF_INCLUDE_DIRS} ${CMAKE_CURRENT_BINARY_DIR})
PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS ../../src/CommunicationMessage.proto)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${PROTO_SRCS} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267")
endif()

add_executable(unit_tests ${sources} ${headers} ${PROTO_SRCS} ${PROTO_HDRS} ../../src/MessageQueue.h
                ../../src/DeliveryHandler.h ../../src/DataSender.h
                ../../src/Node.h ../../src/NodeType.h
                ../../src/Writer.h ../../src/HeartbeatSender.h
                ../../src/DataReceiver.h ../../src/Discoverer.h ../../src/Utilities.h)

TARGET_LINK_LIBRARIES(unit_tests ${PROTOBUF_LITE_LIBRARIES})

if (NOT MSVC)
    target_link_libraries(unit_tests lluf_id lluf_utils ${Boost_LIBRARIES})
endif()

enable_testing()
ADD_TEST(MessageQueueTest unit_tests MessageQueueTest)
SET_SAFIR_TEST_PROPERTIES(MessageQueueTest 120)

ADD_TEST(HeartbeatSenderTest unit_tests HeartbeatSenderTest)
SET_SAFIR_TEST_PROPERTIES(HeartbeatSenderTest 120)

ADD_TEST(DataReceiverTest unit_tests DataReceiverTest)
SET_SAFIR_TEST_PROPERTIES(DataReceiverTest 360)

ADD_TEST(DataSenderTest unit_tests DataSenderTest)
SET_SAFIR_TEST_PROPERTIES(DataSenderTest 120)

#ADD_TEST(DeliveryHandlerTest unit_tests DeliveryHandlerTest)
#SET_SAFIR_TEST_PROPERTIES(DeliveryHandlerTest 120)

ADD_TEST(DiscovererTest unit_tests DiscovererTest)
SET_SAFIR_TEST_PROPERTIES(DiscovererTest 360)
