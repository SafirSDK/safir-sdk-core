FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
ADD_EXECUTABLE(dots_xml_converter ${sources} ${headers})

TARGET_LINK_LIBRARIES(dots_xml_converter PRIVATE
  lluf_config
  lluf_id
  dots_internal
  ${Boost_FILESYSTEM_LIBRARY})

