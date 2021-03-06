FILE(GLOB_RECURSE headers ControlConfig.h ControlUtilsExportDefs.h DoseMainCmd.h ControlCmd.h ControlInfo.h)
set(sources DoseMainCmd.cpp ControlCmd.cpp ControlInfo.cpp)

set(proto_files DoseMainCommands.proto ControlCommands.proto ControlInfo.proto)

#This makes the proto files show up in various IDEs
ADD_CUSTOM_TARGET(dummy_proto_target SOURCES ${proto_files})

PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS ${proto_files})

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${PROTO_SRCS} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267 /wd4018 /wd4512")
endif()

ADD_LIBRARY(control_utils SHARED ${sources} ${headers} ${PROTO_SRCS} ${PROTO_HDRS})

#Make the generated protobuf headers available for us to include inside control_utils
target_include_directories(control_utils PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(control_utils PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(control_utils PRIVATE
  lluf_utils
  ${PROTOBUF_LITE_LIBRARIES}

  PUBLIC
  $<BUILD_INTERFACE:safir_generated-Core-cpp>
  $<BUILD_INTERFACE:dots_internal>
  $<BUILD_INTERFACE:lluf_id>
  $<BUILD_INTERFACE:lluf_config>)

SAFIR_INSTALL(TARGETS control_utils)
