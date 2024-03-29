FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dots_internal SHARED ${sources} ${headers} AnyObject.proto)

PROTOBUF_GENERATE(TARGET dots_internal OUT_VAR generated_files)

if (MSVC)
  SET_SOURCE_FILES_PROPERTIES(${generated_files} PROPERTIES COMPILE_FLAGS "/wd4127 /wd4244 /wd4267 /wd4100")
endif()

TARGET_LINK_LIBRARIES(dots_internal PRIVATE
  lluf_internal
  protobuf::protobuf

  PUBLIC
  lluf_id
  lluf_config
  $<BUILD_INTERFACE:Boost::filesystem>
  $<BUILD_INTERFACE:Boost::thread>
)

SAFIR_INSTALL(TARGETS dots_internal)

INSTALL(PROGRAMS ../scripts/utils/xml_convert.py
  DESTINATION ${SAFIR_INSTALL_DESTINATION_DOC}/utilities/
  COMPONENT Development)
