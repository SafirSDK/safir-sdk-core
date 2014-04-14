find_package(CSharp REQUIRED)
INCLUDE(CSharpMacros)

FILE(GLOB sources *.cs)

SET(CS_FLAGS ${COMMON_CS_FLAGS} "-keyfile:${safir_sdk_core_SOURCE_DIR}/../build/config/sdk/data/build/safirkey.snk")

ADD_CS_LIBRARY(Safir.Logging "${sources}" ALL)

INSTALL(FILES ${target_DLL}
  DESTINATION ${SAFIR_RUNTIME}/bin)
  
INSTALL_CS_DEBUG_INFO()
