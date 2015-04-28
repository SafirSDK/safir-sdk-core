if (NOT CSHARP_FOUND)
  return()
endif()

FILE(GLOB sources *.cs)

ADD_CSHARP_ASSEMBLY(Safir.Dob LIBRARY
  SIGN ${safir-sdk-core_SOURCE_DIR}/src/cmake/safirkey.snk
  SOURCES ${sources}
  REFERENCES Safir.Logging Safir.Dob.Typesystem safir_generated-Core-dotnet
  DLL_IMPORTS dose_dll)

SAFIR_INSTALL(TARGETS Safir.Dob)
