cmake_minimum_required(VERSION 2.8)

project(system_picture_test CXX C) #C is needed by FindProtobuf

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FIND_PACKAGE(Boost COMPONENTS REQUIRED thread filesystem system unit_test_framework)
FIND_PACKAGE(Protobuf REQUIRED)

INCLUDE_DIRECTORIES(${PROTOBUF_INCLUDE_DIRS} ${CMAKE_CURRENT_BINARY_DIR})

PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS 
  ../../src/RawStatisticsMessage.proto 
  ../../src/SystemStateMessage.proto
  ../../src/ElectionMessage.proto)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${PROTO_SRCS} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267")
endif()

#Need to define this to not get inconsistent dll linkage on windows.
#no test is meant to actually link the system_picture library, so we can do this.
add_definitions(-Dsystem_picture_EXPORTS)

ADD_EXECUTABLE(RawStatistics_test RawStatistics_test.cpp ../../src/RawStatistics.cpp ${PROTO_SRCS} ${PROTO_HDRS})
ADD_EXECUTABLE(SystemState_test SystemState_test.cpp ../../src/SystemState.cpp ${PROTO_SRCS} ${PROTO_HDRS})
ADD_EXECUTABLE(RawPublisherLocal_test RawPublisherLocal_test.cpp ${PROTO_HDRS})
ADD_EXECUTABLE(RawPublisherRemote_test RawPublisherRemote_test.cpp ../../src/RawStatistics.cpp ${PROTO_SRCS}  ${PROTO_HDRS})
ADD_EXECUTABLE(StatePublisherLocal_test StatePublisherLocal_test.cpp ${PROTO_HDRS})
ADD_EXECUTABLE(StatePublisherRemote_test StatePublisherRemote_test.cpp ${PROTO_HDRS})

ADD_EXECUTABLE(LocalSubscriber_test LocalSubscriber_test.cpp ../../src/RawStatistics.cpp ${PROTO_SRCS} ${PROTO_HDRS})
ADD_EXECUTABLE(RemoteSubscriber_test RemoteSubscriber_test.cpp)

ADD_EXECUTABLE(RawHandler_test RawHandler_test.cpp ../../src/RawStatistics.cpp ${PROTO_SRCS} ${PROTO_HDRS})
ADD_EXECUTABLE(RawChanges_test RawChanges_test.cpp)
ADD_EXECUTABLE(ElectionHandler_test ElectionHandler_test.cpp ../../src/RawStatistics.cpp ${PROTO_SRCS} ${PROTO_HDRS})
ADD_EXECUTABLE(Coordinator_test Coordinator_test.cpp ../../src/RawStatistics.cpp ${PROTO_SRCS} ${PROTO_HDRS})


TARGET_LINK_LIBRARIES(RawStatistics_test
  ${PROTOBUF_LITE_LIBRARIES})

TARGET_LINK_LIBRARIES(SystemState_test
  ${PROTOBUF_LITE_LIBRARIES})

TARGET_LINK_LIBRARIES(RawPublisherRemote_test
  ${PROTOBUF_LITE_LIBRARIES})

TARGET_LINK_LIBRARIES(LocalSubscriber_test
  ${PROTOBUF_LITE_LIBRARIES})

TARGET_LINK_LIBRARIES(RawHandler_test
  ${PROTOBUF_LITE_LIBRARIES})

TARGET_LINK_LIBRARIES(ElectionHandler_test
  ${PROTOBUF_LITE_LIBRARIES})


TARGET_LINK_LIBRARIES(Coordinator_test
  ${PROTOBUF_LITE_LIBRARIES})


if(NOT MSVC)
  TARGET_LINK_LIBRARIES(RawStatistics_test ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(SystemState_test ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(RawPublisherLocal_test lluf_utils lluf_config ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(RawPublisherRemote_test lluf_id lluf_utils lluf_config ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(StatePublisherLocal_test lluf_utils lluf_config ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(StatePublisherRemote_test lluf_id lluf_utils lluf_config ${Boost_LIBRARIES})

  TARGET_LINK_LIBRARIES(LocalSubscriber_test lluf_utils lluf_config ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(RemoteSubscriber_test lluf_id lluf_utils lluf_config ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(RawHandler_test lluf_utils ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(RawChanges_test ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(ElectionHandler_test lluf_id lluf_utils ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(Coordinator_test lluf_utils ${Boost_LIBRARIES})
endif()

ENABLE_TESTING()



add_test(RawStatistics_test RawStatistics_test)
SET_SAFIR_TEST_PROPERTIES(RawStatistics_test 30)

add_test(SystemState_test SystemState_test)
SET_SAFIR_TEST_PROPERTIES(SystemState_test 30)

add_test(RawPublisherLocal_test RawPublisherLocal_test)
SET_SAFIR_TEST_PROPERTIES(RawPublisherLocal_test 30)

add_test(RawPublisherRemote_test RawPublisherRemote_test)
SET_SAFIR_TEST_PROPERTIES(RawPublisherRemote_test 30)

add_test(StatePublisherLocal_test StatePublisherLocal_test)
SET_SAFIR_TEST_PROPERTIES(StatePublisherLocal_test 30)

add_test(StatePublisherRemote_test StatePublisherRemote_test)
SET_SAFIR_TEST_PROPERTIES(StatePublisherRemote_test 30)


add_test(LocalSubscriber_test LocalSubscriber_test)
SET_SAFIR_TEST_PROPERTIES(LocalSubscriber_test 30)

add_test(RemoteSubscriber_test RemoteSubscriber_test)
SET_SAFIR_TEST_PROPERTIES(RemoteSubscriber_test 30)

add_test(RawHandler_test RawHandler_test)
SET_SAFIR_TEST_PROPERTIES(RawHandler_test 30)

add_test(RawChanges_test RawChanges_test)
SET_SAFIR_TEST_PROPERTIES(RawChanges_test 30)

add_test(ElectionHandler_test ElectionHandler_test)
SET_SAFIR_TEST_PROPERTIES(ElectionHandler_test 1200)

add_test(ElectionHandler_test_with_overflows ElectionHandler_test --overflows)
SET_SAFIR_TEST_PROPERTIES(ElectionHandler_test_with_overflows 1200)

add_test(Coordinator_test Coordinator_test)
SET_SAFIR_TEST_PROPERTIES(Coordinator_test 30)
