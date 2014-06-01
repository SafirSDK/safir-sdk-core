if (NOT CSHARP_FOUND)
  return()
endif()

FILE(GLOB sources *.cs)

ADD_CSHARP_ASSEMBLY(Safir.Time LIBRARY
  SIGN ${safir_sdk_core_SOURCE_DIR}/../build/config/sdk/data/build/safirkey.snk
  SOURCES ${sources}
  REFERENCES Safir.Dob.Typesystem)

INSTALL_CSHARP_ASSEMBLY(TARGET Safir.Time
  DESTINATION lib/safir_sdk_core)

