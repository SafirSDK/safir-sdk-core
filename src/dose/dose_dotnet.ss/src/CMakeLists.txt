cmake_minimum_required(VERSION 2.6)

PROJECT(dose_dotnet)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

find_package(CSharp REQUIRED)
INCLUDE(CSharpMacros)

FILE(GLOB sources *.cs)

SET(CS_FLAGS ${COMMON_CS_FLAGS} "-doc:Safir.Dob.xml" "-keyfile:${SAFIR_SDK}/data/build/safirkey.snk"
                                "-r:${SAFIR_RUNTIME}/bin/Safir.Dob.Typesystem.dll,${SAFIR_RUNTIME}/bin/dots_generated-dotnet.dll,System.Windows.Forms.dll")

ADD_CS_LIBRARY(Safir.Dob "${sources}" ALL)

INSTALL(FILES ${target_DLL}
              Safir.Dob.xml
  DESTINATION ${SAFIR_RUNTIME}/bin)

INSTALL_CS_DEBUG_INFO()
