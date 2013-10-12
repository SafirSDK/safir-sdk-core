project(doxygen NONE)

cmake_minimum_required(VERSION 2.8)

FILE(TO_CMAKE_PATH $ENV{SAFIR_SDK} SAFIR_SDK)
FILE(TO_CMAKE_PATH $ENV{SAFIR_RUNTIME} SAFIR_RUNTIME)

ADD_CUSTOM_TARGET(html ALL
  COMMAND doxygen)

INSTALL(DIRECTORY html
  DESTINATION ${SAFIR_SDK}/docs/)

#just use these variables to avoid some cmake warnings 
#(they're set in the build script)
if(SAFIR_ADA_SUPPORT OR SAFIR_JAVA_SUPPORT)
endif()
