if (NOT CSHARP_FOUND)
  return()
endif()

FILE(GLOB sources *.cs)

ADD_CSHARP_ASSEMBLY(Safir.Dob.Typesystem LIBRARY
  SIGN ${safir-sdk-core_SOURCE_DIR}/src/cmake/safirkey.snk
  SOURCES ${sources}
  REFERENCES Safir.Logging
  DLL_IMPORTS lluf_id dots_kernel)

SAFIR_INSTALL(TARGETS Safir.Dob.Typesystem)
