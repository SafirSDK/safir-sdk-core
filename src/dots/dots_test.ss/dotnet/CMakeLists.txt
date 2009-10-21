cmake_minimum_required(VERSION 2.6)

PROJECT(dots_test_dotnet)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

find_package(CSharp REQUIRED)
INCLUDE(CSharpMacros)

FILE(GLOB sources *.cs)

SET(CS_FLAGS ${COMMON_CS_FLAGS} "-r:${SAFIR_RUNTIME}/bin/Safir.Dob.Typesystem.dll,${SAFIR_RUNTIME}/bin/dots_generated-dotnet.dll" )

ADD_CS_EXECUTABLE(dots_test_dotnet "${sources}" ALL)

#The csharp build renames the exe to XXXX.csexe, so we rename it back when we install
INSTALL(PROGRAMS ${target_EXE} 
 DESTINATION ${SAFIR_RUNTIME}/bin
  RENAME dots_test_dotnet.exe)

INSTALL_CS_DEBUG_INFO()