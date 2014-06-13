cmake_minimum_required(VERSION 2.8)

project(system_picture_test CXX C) #C is needed by FindProtobuf

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

#FIND_PACKAGE(Boost COMPONENTS thread filesystem system)
FIND_PACKAGE(Protobuf REQUIRED)
#FIND_PACKAGE(PythonInterp)
INCLUDE_DIRECTORIES(${PROTOBUF_INCLUDE_DIRS} ${CMAKE_CURRENT_BINARY_DIR})

PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS 
  ../src/NodeStatisticsMessage.proto 
  ../src/SystemStateMessage.proto
  ../src/ElectionMessage.proto)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${PROTO_SRCS} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267")
endif()


ADD_EXECUTABLE(RawStatistics_test RawStatistics_test.cpp ../src/RawStatistics.cpp ${PROTO_SRCS} ${PROTO_HDRS})

TARGET_LINK_LIBRARIES(RawStatistics_test
  ${PROTOBUF_LITE_LIBRARIES})


ENABLE_TESTING()



add_test(RawStatistics_test RawStatistics_test)



