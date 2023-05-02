# Try to find asciidoctor
# Once done, this will define
#
# Asciidoctor_FOUND      - system has asciidoctor or asciidoctorJ
# ASCIIDOCTOR_EXECUTABLE - the a2x executable
# GZIP_EXECUTABLE        - the gzip executable
# DIA_EXECUTABLE         - the dia executable
#
# ADD_ASCIIDOCTOR_MANPAGE(...) which is described below.
# ADD_ASCIIDOCTOR_DIAGRAM(...) which is described below.
#

FIND_PROGRAM(ASCIIDOCTOR_EXECUTABLE NAMES asciidoctor asciidoctorj)
FIND_PROGRAM(GZIP_EXECUTABLE NAMES gzip)
FIND_PROGRAM(DIA_EXECUTABLE NAMES dia HINTS "C:/Program Files (x86)/Dia/bin")

include( FindPackageHandleStandardArgs )
find_package_handle_standard_args(Asciidoctor DEFAULT_MSG ASCIIDOCTOR_EXECUTABLE GZIP_EXECUTABLE DIA_EXECUTABLE)

mark_as_advanced(ASCIIDOCTOR_EXECUTABLE)
mark_as_advanced(GZIP_EXECUTABLE)
mark_as_advanced(DIA_EXECUTABLE)

#
# Add a manpage to build from asciidoc source. This command will
# also add instructions for installing the manpage.
#
# Usage: ADD_ASCIIDOCTOR_MANPAGE(TARGET <target>
#                                COMPONENT <component>)
#
# Exepects to find a source file named TARGET.asciidoc.
# Call like this: add_asciidoc_manpage(sate.1).
# Will generate a manpage "sate.1" from "sate.1.asciidoc", and will also generate
# installation code.
# Will currently only install to man1, so if you use other section this has to be updated.
#
function(ADD_ASCIIDOCTOR_MANPAGE TARGET)
    cmake_parse_arguments(_ad "" "COMPONENT" "" ${ARGN})

    if (NOT TARGET)
      message(FATAL_ERROR "ADD_ASCIIDOCTOR_MANPAGE: TARGET not specified!")
    endif()

    if (NOT _ad_COMPONENT)
      message(FATAL_ERROR "ADD_ASCIIDOCTOR_MANPAGE: COMPONENT not specified!")
    endif()

    if (NOT "${_cs_UNPARSED_ARGUMENTS}" STREQUAL "")
      message(FATAL_ERROR "Unknown argument to ADD_ASCIIDOCTOR_MANPAGE '${_ad_UNPARSED_ARGUMENTS}'")
    endif()

    set(_ad_SOURCE_NAME ${CMAKE_CURRENT_SOURCE_DIR}/${TARGET}.asciidoc)
    set(_ad_INTERMEDIATE_NAME ${CMAKE_CURRENT_BINARY_DIR}/${TARGET})
    set(_ad_TARGET_NAME ${CMAKE_CURRENT_BINARY_DIR}/${TARGET}.gz)

    add_custom_command(
      OUTPUT ${_ad_TARGET_NAME}
      COMMAND ${ASCIIDOCTOR_EXECUTABLE} -b manpage -D ${CMAKE_CURRENT_BINARY_DIR} ${_ad_SOURCE_NAME}
      COMMAND ${GZIP_EXECUTABLE} -f ${_ad_INTERMEDIATE_NAME}
      DEPENDS ${_ad_SOURCE_NAME}
      COMMENT "Generating man page ${TARGET}")

    add_custom_target(${TARGET} ALL DEPENDS ${_ad_TARGET_NAME} SOURCES ${_ad_SOURCE_NAME})

    install(
      FILES ${_ad_TARGET_NAME}
      COMPONENT ${_ad_COMPONENT}
      DESTINATION ${SAFIR_INSTALL_DESTINATION_MAN}/man1)
endfunction()

