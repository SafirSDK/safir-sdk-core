FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

#TODO: is there a more elgant way to do this for the protbuf headers for oos builds?
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})

PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS CommunicationMessage.proto)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${PROTO_SRCS} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267")
endif()

ADD_LIBRARY(communication SHARED ${sources} ${headers} ${PROTO_SRCS} ${PROTO_HDRS} )

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(communication PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(communication
  lluf_id
   lluf_utils
   ${Boost_THREAD_LIBRARY}
   ${Boost_SYSTEM_LIBRARY}
   ${PROTOBUF_LITE_LIBRARIES}
)

SAFIR_INSTALL(TARGETS communication)
