find_package(Doxygen OPTIONAL_COMPONENTS dot)

if (NOT DOXYGEN_FOUND)
  message(STATUS "Failed to find the Doxygen tools, will not build Doxygen documentation")
  return()
endif()

if (ENV{VERBOSE})
  set(CMAKE_DOXYGEN_QUIET "NO")
else()
  set(CMAKE_DOXYGEN_QUIET "YES")
endif()

if (DOXYGEN_VERSION VERSION_EQUAL "1.8.17")
  set(DOXYGEN_WORKAROUNDS "DOXYGEN_BUG_WORKAROUND=1")
endif()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)

FILE(GLOB_RECURSE headers ${safir-sdk-core_SOURCE_DIR}/include/*.h)

add_custom_command(
  OUTPUT documentation_generated
  COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
  COMMAND ${CMAKE_COMMAND} -E touch documentation_generated
  DEPENDS ${headers} Doxyfile.in
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMENT "Generating API documentation with Doxygen")

add_custom_target(Doxygen ALL DEPENDS documentation_generated)

set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES html)

add_dependencies(Doxygen safir_generated-Core-cpp)

INSTALL(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html
  DESTINATION ${SAFIR_INSTALL_DESTINATION_DOC}
  COMPONENT Development)
