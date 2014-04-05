FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
ADD_EXECUTABLE(dots_xml_converter ${sources} ${headers})

TARGET_LINK_LIBRARIES(dots_xml_converter lluf_id dots_internal ${Boost_LIBRARIES})

