FIND_PACKAGE(Boost COMPONENTS filesystem system thread)

ADD_SUBDIRECTORY(override_test)
ADD_SUBDIRECTORY(parser_test)
ADD_SUBDIRECTORY(serialization_test)
add_subdirectory(dots_xml_converter)

