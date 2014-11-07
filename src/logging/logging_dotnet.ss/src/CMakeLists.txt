if (NOT CSHARP_FOUND)
  return()
endif()

ADD_CSHARP_ASSEMBLY(Safir.Logging LIBRARY
  SIGN ${safir_sdk_core_SOURCE_DIR}/build/config/sdk/data/build/safirkey.snk
  SOURCES Log.cs AssemblyInfo.cs
  DLL_IMPORTS logging_library)

SAFIR_INSTALL(TARGETS Safir.Logging)
