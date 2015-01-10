FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

FILE(GLOB proto_files *.proto)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})

PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS ${proto_files})

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${PROTO_SRCS} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267")
endif()

ADD_LIBRARY(control_utils SHARED ${sources} ${headers} ${PROTO_SRCS} ${PROTO_HDRS})

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(control_utils PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(control_utils
  lluf_config
  lluf_id
  lluf_utils
  dots_internal
  #${PROTOBUF_LITE_LIBRARIES}
  )

SAFIR_INSTALL(TARGETS control_utils)
