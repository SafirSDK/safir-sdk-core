cmake_minimum_required(VERSION 2.8)

project(system_picture CXX C) #C is needed by FindProtobuf

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FIND_PACKAGE(Boost COMPONENTS chrono filesystem system)
FIND_PACKAGE(Protobuf REQUIRED)


FILE(GLOB_RECURSE headers *.h)
SET(sources 
  SystemPicture.cpp 
  Coordinator.cpp
  RawStatistics.cpp
  SystemState.cpp
  )

INCLUDE_DIRECTORIES(${PROTOBUF_INCLUDE_DIRS} ${CMAKE_CURRENT_BINARY_DIR})

PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS 
  NodeStatisticsMessage.proto 
  SystemStateMessage.proto
  ElectionMessage.proto)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${PROTO_SRCS} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267")
endif()

add_library(system_picture SHARED ${sources} ${headers} ${PROTO_SRCS} ${PROTO_HDRS})

TARGET_LINK_LIBRARIES(system_picture 
  optimized communication
  debug communication${CMAKE_DEBUG_POSTFIX}
  ${PROTOBUF_LITE_LIBRARIES})

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(system_picture 
    lluf_id
    lluf_utils
    lluf_startup_synchronizer
    lluf_config
    ${Boost_LIBRARIES}
    atomic
    rt)
endif()

FIND_PACKAGE(Boost COMPONENTS program_options)

add_executable(system_picture_listener system_picture_listener.cpp)
target_link_libraries(system_picture_listener 
  optimized system_picture
  debug system_picture${CMAKE_DEBUG_POSTFIX}
  ${Boost_LIBRARIES})

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS system_picture system_picture_listener
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(system_picture)
