find_package(Doxygen)

if (NOT DOXYGEN_FOUND)
  message(WARNING "Failed to find the Doxygen tools, will not build Doxygen documentation")
  return()
endif()

#TODO: we need all the public headers to be in one useful folder so that we can run doxygen on them

#ADD_CUSTOM_TARGET(Doxygen ALL
#  COMMAND doxygen)

#INSTALL(DIRECTORY html
#  DESTINATION ${SAFIR_SDK}/docs/)

