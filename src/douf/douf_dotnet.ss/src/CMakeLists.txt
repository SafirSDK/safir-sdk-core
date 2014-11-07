if (NOT CSHARP_FOUND)
  return()
endif()

FILE(GLOB sources *.cs)

ADD_CSHARP_ASSEMBLY(Safir.Time LIBRARY
  SIGN ${safir_sdk_core_SOURCE_DIR}/build/config/sdk/data/build/safirkey.snk
  SOURCES ${sources}
  REFERENCES Safir.Dob.Typesystem
  DLL_IMPORTS douf_time_library)

SAFIR_INSTALL(TARGETS Safir.Time)

