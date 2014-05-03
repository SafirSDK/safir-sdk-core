cmake_minimum_required(VERSION 2.8)

PROJECT(swre_interface_dotnet NONE)


INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

find_package(CSharp REQUIRED)
INCLUDE(CSharpMacros)

FILE(GLOB sources *.cs)

SET(CS_FLAGS ${COMMON_CS_FLAGS} "-keyfile:${SAFIR_SDK}/data/build/safirkey.snk"
                                "-r:${SAFIR_RUNTIME}/bin/Safir.Dob.Typesystem.dll,${SAFIR_RUNTIME}/bin/Safir.Dob.dll,${SAFIR_RUNTIME}/bin/dots_generated-dotnet.dll,${SAFIR_RUNTIME}/bin/Safir.Logging.dll")

ADD_CS_LIBRARY(Safir.SwReports "${sources}" ALL)

# INSTALL(FILES ${target_DLL}
#   DESTINATION ${SAFIR_RUNTIME}/bin)
  
# INSTALL_CS_DEBUG_INFO()
