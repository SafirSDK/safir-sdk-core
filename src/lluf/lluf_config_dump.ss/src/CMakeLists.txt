cmake_minimum_required(VERSION 2.8)

project(lluf_config_dump CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(safir_show_config ${sources})

FIND_PACKAGE(Boost COMPONENTS program_options)


if(NOT MSVC)
  TARGET_LINK_LIBRARIES(safir_show_config lluf_config ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS safir_show_config  
  DESTINATION ${SAFIR_RUNTIME}/bin)
