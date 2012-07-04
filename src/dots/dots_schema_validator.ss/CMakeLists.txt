cmake_minimum_required(VERSION 2.6)

PROJECT(dob_schema_validator NONE)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

find_package(CSharp REQUIRED)
INCLUDE(CSharpMacros)

FILE(GLOB sources *.cs)

ADD_CS_EXECUTABLE(dob_schema_validator "${sources}" ALL)

INSTALL(PROGRAMS ${target_EXE}
  DESTINATION ${SAFIR_RUNTIME}/bin
  RENAME dob_schema_validator.exe)

INSTALL_CS_DEBUG_INFO()
