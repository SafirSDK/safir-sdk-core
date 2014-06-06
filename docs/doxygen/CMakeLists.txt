find_package(Doxygen)

if (NOT DOXYGEN_FOUND)
  message(WARNING "Failed to find the Doxygen tools, will not build Doxygen documentation")
  return()
endif()

ADD_CUSTOM_TARGET(Doxygen ALL
  COMMAND doxygen)

INSTALL(DIRECTORY html
  DESTINATION share/doc/safir_sdk_core)

