if (NOT CSHARP_FOUND)
  return()
endif()

FILE(GLOB sources *.cs)

#SET(CS_FLAGS ${COMMON_CS_FLAGS} "-doc:Safir.Dob.xml" "-keyfile:${SAFIR_SDK}/data/build/safirkey.snk"
#                                "-r:${SAFIR_RUNTIME}/bin/Safir.Logging.dll,${SAFIR_RUNTIME}/bin/Safir.Dob.Typesystem.dll,${SAFIR_RUNTIME}/bin/dots_generated-dotnet.dll")

#ADD_CS_LIBRARY(Safir.Dob "${sources}" ALL)

ADD_CSHARP_ASSEMBLY(Safir.Dob LIBRARY
  SIGN ${safir_sdk_core_SOURCE_DIR}/../build/config/sdk/data/build/safirkey.snk
  SOURCES ${sources}
  REFERENCES Safir.Logging Safir.Dob.Typesystem dots_generated-Safir-dotnet)

#TODO: install

# INSTALL(FILES ${target_DLL}
#               Safir.Dob.xml
#   DESTINATION ${SAFIR_RUNTIME}/bin)

# INSTALL_CS_DEBUG_INFO()
