if (NOT CSHARP_FOUND)
  return()
endif()

FILE(GLOB sources *.cs)

ADD_CSHARP_ASSEMBLY(Safir.Dob LIBRARY
  SIGN ${safir_sdk_core_SOURCE_DIR}/build/config/sdk/data/build/safirkey.snk
  SOURCES ${sources}
  REFERENCES Safir.Logging Safir.Dob.Typesystem safir_generated-Core-dotnet)

add_dependencies(Safir.Dob dose_dll)

SAFIR_INSTALL(TARGETS Safir.Dob)
