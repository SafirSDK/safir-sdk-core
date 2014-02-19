FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
ADD_EXECUTABLE(dots_xml_converter ${sources} ${headers})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dots_xml_converter dots_id dots_internal ${Boost_LIBRARIES})
endif()

