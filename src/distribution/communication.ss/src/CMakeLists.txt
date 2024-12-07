FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(communication SHARED ${sources} ${headers} CommunicationMessage.proto)

PROTOBUF_GENERATE(TARGET communication OUT_VAR generated_files)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${generated_files} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267 /wd4100 /wd4702")
endif()

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(communication PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(communication PRIVATE
  lluf_id
  lluf_internal
  protobuf::protobuf
  Boost::regex)

SAFIR_INSTALL(TARGETS communication)



