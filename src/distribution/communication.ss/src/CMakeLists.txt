FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS CommunicationMessage.proto)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${PROTO_SRCS} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267 /wd4018")
endif()

ADD_LIBRARY(communication SHARED ${sources} ${headers} ${PROTO_SRCS} ${PROTO_HDRS} )

#Make the generated protobuf headers available for us to include inside communication
target_include_directories(communication PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(communication PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(communication PRIVATE
  lluf_id
  lluf_utils
  ${PROTOBUF_LITE_LIBRARIES})

SAFIR_INSTALL(TARGETS communication)



