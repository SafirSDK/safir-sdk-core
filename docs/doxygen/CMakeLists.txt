find_package(Doxygen)

if (NOT DOXYGEN_FOUND)
  message(WARNING "Failed to find the Doxygen tools, will not build Doxygen documentation")
  return()
endif()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)

add_custom_target(Doxygen ALL
  ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMENT "Generating API documentation with Doxygen" VERBATIM
  )

INSTALL(DIRECTORY html
  DESTINATION share/doc/safir_sdk_core)

set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES html)

#TODO: can we get the dou-headers in here as well?
