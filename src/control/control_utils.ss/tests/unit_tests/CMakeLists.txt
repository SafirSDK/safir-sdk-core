cmake_minimum_required(VERSION 2.8)

project(control_utils_test CXX C) #C is needed by FindProtobuf

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FIND_PACKAGE(Boost COMPONENTS timer thread filesystem program_options system unit_test_framework)
FIND_PACKAGE(Protobuf REQUIRED)
FIND_PACKAGE(PythonInterp)

INCLUDE_DIRECTORIES(${PROTOBUF_INCLUDE_DIRS} ${CMAKE_CURRENT_BINARY_DIR})

PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS 
  ../../src/ControlCommands.proto)
  
if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${PROTO_SRCS} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267")
endif()  

#Need to define this to not get inconsistent dll linkage on windows.
#no test is meant to actually link the system_picture library, so we can do this.
add_definitions(-Dcontrol_utils_EXPORTS)

ADD_EXECUTABLE(DoseMainCmd_test DoseMainCmd_test.cpp ../../src/DoseMainCmd.cpp ${PROTO_SRCS} ${PROTO_HDRS})

TARGET_LINK_LIBRARIES(DoseMainCmd_test
  ${PROTOBUF_LITE_LIBRARIES})
  
if(NOT MSVC)
  TARGET_LINK_LIBRARIES(DoseMainCmd_test lluf_config ${Boost_LIBRARIES})
endif()  
  
ENABLE_TESTING()

add_test(DoseMainCmd_test DoseMainCmd_test)

ADD_SUBDIRECTORY(Config_test)


