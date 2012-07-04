cmake_minimum_required(VERSION 2.6)
project(VehicleMmiCsWinForms NONE)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)
find_package(CSharp REQUIRED)
INCLUDE(CSharpMacros)

FILE(GLOB sources *.cs Properties/*.cs)
FILE(GLOB resx_files *.resx)
FILE(GLOB property_resx_files Properties/*.resx)


SET(CS_FLAGS ${COMMON_CS_FLAGS} "-r:${SAFIR_RUNTIME}/bin/Safir.Dob.Typesystem.dll,${SAFIR_RUNTIME}/bin/Safir.Dob.dll,${SAFIR_RUNTIME}/bin/dots_generated-dotnet.dll,${SAFIR_RUNTIME}/bin/Safir.SwReports.dll,System.Windows.Forms.dll,System.Drawing.dll" )

ADD_CS_RESOURCES("${resx_files}" VehicleMmiCsWinForms .)
ADD_CS_RESOURCES("${property_resx_files}" VehicleMmiCsWinForms.Properties ${CMAKE_CURRENT_SOURCE_DIR}/Properties)
ADD_CS_GUI_EXECUTABLE(VehicleMmiCsWinForms "${sources}" ALL)

#The csharp build renames the exe to XXXX.csexe, so we rename it back when we install
INSTALL(PROGRAMS ${target_EXE} 
 DESTINATION ${SAFIR_RUNTIME}/bin
  RENAME VehicleMmiCsWinForms.exe)

INSTALL_CS_DEBUG_INFO()
