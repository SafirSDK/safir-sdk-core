cmake_minimum_required(VERSION 2.6)

PROJECT (logger_control)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(logger_control ${sources})
TARGET_LINK_LIBRARIES(logger_control lluf_utils ${COMMON_LIBRARIES})

INSTALL(TARGETS logger_control 
  DESTINATION ${SAFIR_RUNTIME}/bin)
