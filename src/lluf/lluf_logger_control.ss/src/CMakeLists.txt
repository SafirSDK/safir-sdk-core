cmake_minimum_required(VERSION 2.8)

project(logger_control CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(logger_control ${sources})

FIND_PACKAGE(Boost COMPONENTS filesystem program_options system)

if(NOT MSVC)
  TARGET_LINK_LIBRARIES(logger_control lluf_utils ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS logger_control 
  DESTINATION ${SAFIR_RUNTIME}/bin)
