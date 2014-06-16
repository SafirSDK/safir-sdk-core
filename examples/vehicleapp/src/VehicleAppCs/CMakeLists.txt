cmake_minimum_required(VERSION 2.8)

project(VehicleAppCs CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)
find_package(CSharp REQUIRED)
INCLUDE(CSharpMacros)

FILE(GLOB sources *.cs)

SET(CS_FLAGS ${COMMON_CS_FLAGS} "-r:${SAFIR_RUNTIME}/bin/Safir.Dob.Typesystem.dll,${SAFIR_RUNTIME}/bin/Safir.Dob.dll,${SAFIR_RUNTIME}/bin/safir_generated-dotnet.dll,${SAFIR_RUNTIME}/bin/Safir.SwReports.dll,${SAFIR_RUNTIME}/bin/Safir.Logging.dll" )

ADD_CS_EXECUTABLE(VehicleAppCs "${sources}" ALL)

#The csharp build renames the exe to XXXX.csexe, so we rename it back when we install
INSTALL(PROGRAMS ${target_EXE} 
 DESTINATION ${SAFIR_RUNTIME}/bin
  RENAME VehicleAppCs.exe)

INSTALL_CS_DEBUG_INFO()

