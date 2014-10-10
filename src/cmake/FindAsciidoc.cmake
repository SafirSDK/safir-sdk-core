# Try to find asciidoc
# Once done, this will define
#
# Asciidoc_FOUND        - system has asciidoc
# A2X_EXECUTABLE        - the a2x executable

FIND_PROGRAM(A2X_EXECUTABLE NAMES a2x)
FIND_PROGRAM(GZIP_EXECUTABLE NAMES gzip)

include( FindPackageHandleStandardArgs )
find_package_handle_standard_args(Asciidoc DEFAULT_MSG A2X_EXECUTABLE GZIP_EXECUTABLE)

mark_as_advanced(A2X_EXECUTABLE)
mark_as_advanced(GZIP_EXECUTABLE)

#exepects to find a source file named TARGET.asciidoc
#call like this add_asciidoc_manpage(sate.1). Will generate
#a manpage sate.1 from sate.1.asciidoc
#will also generate installation code
#will currently only install to man1, so if you use other
#section this has to be updated.
function(ADD_ASCIIDOC_MANPAGE TARGET)
    cmake_parse_arguments(_ad "" "COMPONENT" "" ${ARGN})

    if (NOT TARGET)
      message(FATAL_ERROR "ADD_ASCIIDOC_MANPAGE: TARGET not specified!")
    endif()

    if (NOT _ad_COMPONENT)
      message(FATAL_ERROR "ADD_ASCIIDOC_MANPAGE: COMPONENT not specified!")
    endif()

    if (NOT "${_cs_UNPARSED_ARGUMENTS}" STREQUAL "")
      message(FATAL_ERROR "Unknown argument to ADD_ASCIIDOC_MANPAGE '${_ad_UNPARSED_ARGUMENTS}'")
    endif()

    set(_ad_SOURCE_NAME ${CMAKE_CURRENT_SOURCE_DIR}/${TARGET}.asciidoc)
    set(_ad_COPIED_NAME ${CMAKE_CURRENT_BINARY_DIR}/${TARGET}.asciidoc.copied)
    set(_ad_INTERMEDIATE_NAME ${CMAKE_CURRENT_BINARY_DIR}/${TARGET})
    set(_ad_TARGET_NAME ${CMAKE_CURRENT_BINARY_DIR}/${TARGET}.gz)

    add_custom_command(
      OUTPUT ${_ad_TARGET_NAME} ${_ad_COPIED_NAME}
      COMMAND ${CMAKE_COMMAND} -E copy ${_ad_SOURCE_NAME} ${_ad_COPIED_NAME}
      COMMAND ${A2X_EXECUTABLE} --doctype manpage --format manpage ${_ad_COPIED_NAME}
      COMMAND ${GZIP_EXECUTABLE} -f ${_ad_INTERMEDIATE_NAME}
      DEPENDS ${_ad_SOURCE_NAME}
      COMMENT "Generating man page ${TARGET}")

    add_custom_target(${TARGET} ALL DEPENDS ${_ad_TARGET_NAME})

    install(
      FILES ${_ad_TARGET_NAME}
      COMPONENT ${_ad_COMPONENT}
      DESTINATION ${SAFIR_INSTALL_DESTINATION_MAN}/man1)
endfunction()
