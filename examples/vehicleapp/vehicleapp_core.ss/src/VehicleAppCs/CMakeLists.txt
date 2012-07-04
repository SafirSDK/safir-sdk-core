cmake_minimum_required(VERSION 2.6)

project(VehicleAppCs CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)
find_package(CSharp REQUIRED)
INCLUDE(CSharpMacros)

FILE(GLOB sources *.cs)

SET(CS_FLAGS ${COMMON_CS_FLAGS} "-r:${SAFIR_RUNTIME}/bin/Safir.Dob.Typesystem.dll,${SAFIR_RUNTIME}/bin/Safir.Dob.dll,${SAFIR_RUNTIME}/bin/dots_generated-dotnet.dll,${SAFIR_RUNTIME}/bin/Safir.SwReports.dll" )

ADD_CS_EXECUTABLE(VehicleAppCs "${sources}" ALL)

#The csharp build renames the exe to XXXX.csexe, so we rename it back when we install
INSTALL(PROGRAMS ${target_EXE} 
 DESTINATION ${SAFIR_RUNTIME}/bin
  RENAME VehicleAppCs.exe)

INSTALL_CS_DEBUG_INFO()

