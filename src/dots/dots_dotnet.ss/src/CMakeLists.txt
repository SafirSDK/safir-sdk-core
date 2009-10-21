cmake_minimum_required(VERSION 2.6)

PROJECT(dots_dotnet)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

find_package(CSharp REQUIRED)
INCLUDE(CSharpMacros)

FILE(GLOB sources *.cs)

SET(CS_FLAGS ${COMMON_CS_FLAGS} "-keyfile:${SAFIR_SDK}/data/build/safirkey.snk")

ADD_CS_LIBRARY(Safir.Dob.Typesystem "${sources}" ALL)

INSTALL(FILES ${target_DLL}
  DESTINATION ${SAFIR_RUNTIME}/bin)
  
INSTALL_CS_DEBUG_INFO()
