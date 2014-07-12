if (NOT WIN32)
  return()
endif()

ADD_CSHARP_ASSEMBLY(pathed EXE
  SIGN ${safir_sdk_core_SOURCE_DIR}/build/config/sdk/data/build/safirkey.snk
  SOURCES pathed.cs AppVersion.cs AssemblyInfo.cs InputArgs.cs StringList.cs resource.Designer.cs
  RESOURCES resource.resx)