cmake_minimum_required(VERSION 2.8)

project(no_naughty_includes CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

add_custom_command(
  OUTPUT all_headers_included.cpp 
  COMMAND ${CMAKE_COMMAND} 
  ARGS -P generate.cmake
  DEPENDS generate.cmake)

add_executable(dummy all_headers_included.cpp)

FIND_PACKAGE(Boost COMPONENTS system)

TARGET_LINK_LIBRARIES(dummy ${COMMON_LIBRARIES})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dummy ${Boost_LIBRARIES})
endif()
