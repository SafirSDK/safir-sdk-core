if (NOT CSHARP_FOUND)
  return()
endif()

#SET(CS_FLAGS ${COMMON_CS_FLAGS} "-doc:Safir.Logging.xml" "-keyfile:${safir_sdk_core_SOURCE_DIR}/../build/config/sdk/data/build/safirkey.snk")

ADD_CSHARP_ASSEMBLY(Safir.Logging LIBRARY 
  SIGN ${safir_sdk_core_SOURCE_DIR}/../build/config/sdk/data/build/safirkey.snk
  SOURCES Log.cs AssemblyInfo.cs)

# INSTALL(FILES 
#   ${target_DLL}
#   Safir.Logging.xml
#   DESTINATION ${SAFIR_RUNTIME}/bin)
  
# INSTALL_CS_DEBUG_INFO()
