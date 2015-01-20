FILE(GLOB_RECURSE headers *.h)
SET(sources
  SystemPicture.cpp
  RawStatistics.cpp
  SystemState.cpp
  )

PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS
  RawStatisticsMessage.proto
  SystemStateMessage.proto
  ElectionMessage.proto)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${PROTO_SRCS} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267 /wd4018")
endif()

add_library(system_picture SHARED ${sources} ${headers} ${PROTO_SRCS} ${PROTO_HDRS})

#Make the generated protobuf headers available for us to include inside system_picture
target_include_directories(system_picture PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(system_picture PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(system_picture PRIVATE
  lluf_utils
  lluf_id
  ${PROTOBUF_LITE_LIBRARIES}

  PUBLIC
  $<BUILD_INTERFACE:communication>)

add_executable(system_picture_listener system_picture_listener.cpp)

target_link_libraries(system_picture_listener PRIVATE
  system_picture
  lluf_utils
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

SAFIR_INSTALL(TARGETS system_picture system_picture_listener)
