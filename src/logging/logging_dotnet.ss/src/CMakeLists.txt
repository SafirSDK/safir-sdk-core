if (NOT CSHARP_FOUND)
  return()
endif()

ADD_CSHARP_ASSEMBLY(Safir.Logging LIBRARY 
  SIGN ${safir_sdk_core_SOURCE_DIR}/../build/config/sdk/data/build/safirkey.snk
  SOURCES Log.cs AssemblyInfo.cs)

INSTALL_CSHARP_ASSEMBLY(TARGET Safir.Logging
  DESTINATION lib/safir_sdk_core)

