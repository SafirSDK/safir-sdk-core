cmake_minimum_required(VERSION 2.6)

PROJECT (logger_control)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(logger_control ${sources})

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(logger_control lluf_config ${COMMON_LIBRARIES})
endif()

INSTALL(TARGETS logger_control 
  DESTINATION ${SAFIR_RUNTIME}/bin)
