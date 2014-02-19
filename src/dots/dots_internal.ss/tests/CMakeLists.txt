cmake_minimum_required(VERSION 2.8)

project(dots_internal_tests CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

#if we're using a very old GCC we need to disable some spurious warnings.
if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.5)
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-array-bounds -Wno-strict-aliasing")
endif()

FIND_PACKAGE(Boost COMPONENTS filesystem system thread)

ENABLE_TESTING()
ADD_SUBDIRECTORY(override_test)
ADD_SUBDIRECTORY(parser_test)
ADD_SUBDIRECTORY(serialization_test)
add_subdirectory(dots_xml_converter)

