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


PROTOBUF_GENERATE(TARGET system_picture)


#Make the generated protobuf headers available for us to include inside system_picture
target_include_directories(system_picture PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(system_picture PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(system_picture PRIVATE
  lluf_utils
  lluf_id
  protobuf::protobuf

  PUBLIC
  $<BUILD_INTERFACE:communication>)

add_executable(system_picture_listener system_picture_listener.cpp)

target_link_libraries(system_picture_listener PRIVATE
  system_picture
  lluf_utils
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

SAFIR_INSTALL(TARGETS system_picture)
SAFIR_INSTALL(TARGETS system_picture_listener COMPONENT Development)
