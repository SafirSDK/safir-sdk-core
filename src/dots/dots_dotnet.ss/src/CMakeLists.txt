if (NOT CSHARP_FOUND)
  return()
endif()

FILE(GLOB sources *.cs)

SET(CS_FLAGS ${COMMON_CS_FLAGS} "-doc:Safir.Dob.Typesystem.xml" "-keyfile:${safir_sdk_core_SOURCE_DIR}/../build/config/sdk/data/build/safirkey.snk")

ADD_CS_LIBRARY(Safir.Dob.Typesystem "${sources}" ALL)

INSTALL(FILES ${target_DLL}
              Safir.Dob.Typesystem.xml
  DESTINATION ${SAFIR_RUNTIME}/bin)
  
INSTALL_CS_DEBUG_INFO()
