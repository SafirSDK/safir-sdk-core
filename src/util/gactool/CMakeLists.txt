#This tool is only useful on windows.
if (NOT WIN32)
  return()
endif()

ADD_CSHARP_ASSEMBLY(gactool EXE
  SIGN ${safir-sdk-core_SOURCE_DIR}/build/config/sdk/data/build/safirkey.snk
  SOURCES gactool.cs AssemblyInfo.cs)

INSTALL_CSHARP_ASSEMBLY(TARGET gactool DESTINATION ${SAFIR_INSTALL_DESTINATION_INST_UTIL})
