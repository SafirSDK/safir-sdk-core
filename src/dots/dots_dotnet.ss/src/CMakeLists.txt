if (NOT CSHARP_FOUND)
  return()
endif()

FILE(GLOB sources *.cs)

ADD_CSHARP_ASSEMBLY(Safir.Dob.Typesystem LIBRARY
  SIGN ${safir_sdk_core_SOURCE_DIR}/build/config/sdk/data/build/safirkey.snk
  SOURCES ${sources})

add_dependencies(Safir.Dob.Typesystem dots_kernel)

SAFIR_INSTALL(TARGETS Safir.Dob.Typesystem)
