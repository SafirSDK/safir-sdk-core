FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS AnyObject.proto)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${PROTO_SRCS} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267 /wd4018")
endif()

ADD_LIBRARY(dots_internal SHARED ${sources} ${headers} ${PROTO_SRCS} ${PROTO_HDRS})

#Make the generated protobuf headers available for us to include inside control_utils
target_include_directories(dots_internal PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

TARGET_LINK_LIBRARIES(dots_internal PRIVATE
  lluf_utils
  ${PROTOBUF_LITE_LIBRARIES}

  PUBLIC
  lluf_id
  lluf_config
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY}
)

SAFIR_INSTALL(TARGETS dots_internal)

INSTALL(PROGRAMS ../scripts/utils/xml_convert.py
  DESTINATION ${SAFIR_INSTALL_DESTINATION_DOC}/utilities/
  COMPONENT Development)
