if (NOT CSHARP_FOUND)
  return()
endif()

ADD_CSHARP_ASSEMBLY(Safir.Logging LIBRARY
  SIGN ${safir-sdk-core_SOURCE_DIR}/src/cmake/safirkey.snk
  SOURCES Log.cs AssemblyInfo.cs
  DLL_IMPORTS logging_library)

SAFIR_INSTALL(TARGETS Safir.Logging)
