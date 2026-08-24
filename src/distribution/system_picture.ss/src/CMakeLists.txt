FILE(GLOB_RECURSE headers *.h)
SET(sources
  SystemPicture.cpp
  RawStatistics.cpp
  SystemState.cpp
  )


add_library(system_picture SHARED ${sources} ${headers}
  RawStatisticsMessage.proto
  SystemStateMessage.proto
  ElectionMessage.proto)
safir_mark_dual_abi(system_picture)


PROTOBUF_GENERATE(TARGET system_picture OUT_VAR generated_files)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${generated_files} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267 /wd4100 /wd4702 /wd4141 /wd4189")
endif()

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(system_picture PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(system_picture PRIVATE
  lluf_internal
  lluf_id
  protobuf::protobuf

  PUBLIC
  $<BUILD_INTERFACE:communication>)

add_executable(system_picture_listener system_picture_listener.cpp)

target_link_libraries(system_picture_listener PRIVATE
  system_picture
  lluf_internal
  Boost::program_options)

SAFIR_INSTALL(TARGETS system_picture)
#system_picture_listener is a runtime diagnostic CLI, so it belongs in the Tools
#component (not Development). It also needs to be present for the installed
#TestSuite: the incarnation_and_control test drives it, and the testsuite package
#Depends on the tools package, so shipping it in Tools makes it available there
#without duplicating the binary across two packages.
SAFIR_INSTALL(TARGETS system_picture_listener COMPONENT Tools)
