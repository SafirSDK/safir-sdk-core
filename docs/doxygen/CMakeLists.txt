find_package(Doxygen)

if (NOT DOXYGEN_FOUND)
  message(STATUS "Failed to find the Doxygen tools, will not build Doxygen documentation")
  return()
endif()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)

add_custom_target(Doxygen ALL
  ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMENT "Generating API documentation with Doxygen" VERBATIM
  )

set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES html)

add_dependencies(Doxygen dots_generated-Safir-cpp dots_generated-olib-cpp dots_generated-swre_library-cpp dots_generated-foreach-cpp dots_generated-douf_time-cpp)

INSTALL(DIRECTORY html
  DESTINATION share/doc/safir_sdk_core)

#TODO: do proper depependency handling, currently this is rebuilt every time, which is annoying
