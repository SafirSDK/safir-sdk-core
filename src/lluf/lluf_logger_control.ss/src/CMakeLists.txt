cmake_minimum_required(VERSION 2.6)

project(logger_control CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(logger_control ${sources})

FIND_PACKAGE(Boost COMPONENTS filesystem program_options system)
TARGET_LINK_LIBRARIES(logger_control ${COMMON_LIBRARIES})

if(NOT MSVC)
  TARGET_LINK_LIBRARIES(logger_control lluf_utils ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS logger_control 
  DESTINATION ${SAFIR_RUNTIME}/bin)
