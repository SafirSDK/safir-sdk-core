cmake_minimum_required(VERSION 2.8)

project(system_picture_test CXX C) #C is needed by FindProtobuf

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FIND_PACKAGE(Boost COMPONENTS REQUIRED filesystem system unit_test_framework)
FIND_PACKAGE(Protobuf REQUIRED)

INCLUDE_DIRECTORIES(${PROTOBUF_INCLUDE_DIRS} ${CMAKE_CURRENT_BINARY_DIR})

PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS 
  ../src/NodeStatisticsMessage.proto 
  ../src/SystemStateMessage.proto
  ../src/ElectionMessage.proto)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${PROTO_SRCS} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267")
endif()

#Need to define this to not get inconsistent dll linkage on windows.
#no test is meant to actually link the system_picture library, so we can do this.
add_definitions(-Dsystem_picture_EXPORTS)

ADD_EXECUTABLE(RawStatistics_test RawStatistics_test.cpp ../src/RawStatistics.cpp ${PROTO_SRCS} ${PROTO_HDRS})
ADD_EXECUTABLE(SystemState_test SystemState_test.cpp ../src/SystemState.cpp ${PROTO_SRCS} ${PROTO_HDRS})
ADD_EXECUTABLE(RawPublisherLocal_test RawPublisherLocal_test.cpp ${PROTO_HDRS})
ADD_EXECUTABLE(StatePublisherLocal_test StatePublisherLocal_test.cpp ${PROTO_HDRS})

ADD_EXECUTABLE(LocalSubscriber_test LocalSubscriber_test.cpp ../src/RawStatistics.cpp ${PROTO_SRCS} ${PROTO_HDRS})
ADD_EXECUTABLE(RemoteSubscriber_test RemoteSubscriber_test.cpp)

TARGET_LINK_LIBRARIES(RawStatistics_test
  ${PROTOBUF_LITE_LIBRARIES})

TARGET_LINK_LIBRARIES(SystemState_test
  ${PROTOBUF_LITE_LIBRARIES})

TARGET_LINK_LIBRARIES(LocalSubscriber_test
  ${PROTOBUF_LITE_LIBRARIES})


if(NOT MSVC)
  TARGET_LINK_LIBRARIES(RawStatistics_test ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(SystemState_test ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(RawPublisherLocal_test lluf_utils lluf_config ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(StatePublisherLocal_test lluf_utils lluf_config ${Boost_LIBRARIES})

  TARGET_LINK_LIBRARIES(LocalSubscriber_test lluf_utils lluf_config ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(RemoteSubscriber_test lluf_id lluf_utils lluf_config ${Boost_LIBRARIES})
endif()

ENABLE_TESTING()



add_test(RawStatistics_test RawStatistics_test)
add_test(SystemState_test SystemState_test)

add_test(RawPublisherLocal_test RawPublisherLocal_test)
set_tests_properties(RawPublisherLocal_test PROPERTIES TIMEOUT 30)

add_test(StatePublisherLocal_test StatePublisherLocal_test)
set_tests_properties(StatePublisherLocal_test PROPERTIES TIMEOUT 30)



add_test(LocalSubscriber_test LocalSubscriber_test)
set_tests_properties(LocalSubscriber_test PROPERTIES TIMEOUT 30)

add_test(RemoteSubscriber_test RemoteSubscriber_test)
set_tests_properties(RemoteSubscriber_test PROPERTIES TIMEOUT 30)
