cmake_minimum_required(VERSION 2.6)

PROJECT(dose_sate NONE)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

find_package(CSharp REQUIRED)
INCLUDE(CSharpMacros)

FILE(GLOB sources *.cs)
FILE(GLOB resx_files *.resx)
SET(CS_FLAGS ${COMMON_CS_FLAGS} "-r:${SAFIR_RUNTIME}/bin/Safir.Dob.Typesystem.dll,${SAFIR_RUNTIME}/bin/dots_generated-dotnet.dll,${SAFIR_RUNTIME}/bin/Safir.Dob.dll,System.Windows.Forms.dll,System.Drawing.dll,System.Data.dll" )

ADD_CS_RESOURCES("${resx_files}" Sate .)
ADD_CS_GUI_EXECUTABLE(sate "${sources}" ALL)

#The csharp build renames the exe to sate.csexe, so we rename it back when we install
INSTALL(PROGRAMS ${target_EXE}
  DESTINATION ${SAFIR_RUNTIME}/bin
  RENAME sate.exe)

INSTALL_CS_DEBUG_INFO()
