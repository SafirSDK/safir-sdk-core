FILE(GLOB_RECURSE headers ControlConfig.h ControlUtilsExportDefs.h DoseMainCmd.h ControlCmd.h ControlInfo.h)
set(sources DoseMainCmd.cpp ControlCmd.cpp ControlInfo.cpp)
set(proto_files DoseMainCommands.proto ControlCommands.proto ControlInfo.proto)


ADD_LIBRARY(control_utils SHARED ${sources} ${headers} ${proto_files})

PROTOBUF_GENERATE(TARGET control_utils OUT_VAR generated_files)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${generated_files} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267")
endif()

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(control_utils PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(control_utils PRIVATE
  lluf_internal
  protobuf::protobuf

  PUBLIC
  $<BUILD_INTERFACE:safir_generated-Core-cpp>
  $<BUILD_INTERFACE:dots_internal>
  $<BUILD_INTERFACE:lluf_id>
  $<BUILD_INTERFACE:lluf_config>)

SAFIR_INSTALL(TARGETS control_utils)
