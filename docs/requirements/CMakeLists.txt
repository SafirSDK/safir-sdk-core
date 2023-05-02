if (NOT ASCIIDOCTOR_FOUND)
  message (STATUS "Asciidoctor not found, not generating requirements specification")
  return()
endif()

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/images)

add_custom_command(
  OUTPUT requirements_specification.html
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/../logo/renders/logotype256.png images/
  COMMAND ${ASCIIDOCTOR_EXECUTABLE}  -b html5
          -a stylesheet=safir-stylesheet.css
          -a SAFIR_VERSION=${SAFIR_VERSION_STRING}
          -D ${CMAKE_CURRENT_BINARY_DIR}
          ${CMAKE_CURRENT_SOURCE_DIR}/requirements_specification.txt
  DEPENDS requirements_specification.txt safir-stylesheet.css
  COMMENT "Generating Requirement Specification HTML")

INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/requirements_specification.html
  DESTINATION ${SAFIR_INSTALL_DESTINATION_DOC}
  COMPONENT Development)

FIND_PROGRAM(DBLATEX_EXECUTABLE NAMES dblatex)

if(DBLATEX_EXECUTABLE)
  add_custom_command(
    OUTPUT requirements_specification.pdf
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/../logo/renders/logotype1024.png images/
    COMMAND ${ASCIIDOCTOR_EXECUTABLE}  -b docbook
            -a SAFIR_VERSION=${SAFIR_VERSION_STRING}
            -D ${CMAKE_CURRENT_BINARY_DIR}
            ${CMAKE_CURRENT_SOURCE_DIR}/requirements_specification.txt
    COMMAND ${DBLATEX_EXECUTABLE}
            -p ${CMAKE_CURRENT_SOURCE_DIR}/asciidoc-dblatex.xsl
            -s ${CMAKE_CURRENT_SOURCE_DIR}/custom-coverpage-dblatex.sty
            ${CMAKE_CURRENT_BINARY_DIR}/requirements_specification.xml
    DEPENDS requirements_specification.txt custom-coverpage-dblatex.sty asciidoc-dblatex.xsl
    COMMENT "Generating Requirement Specification PDF")

  INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/requirements_specification.pdf
    DESTINATION ${SAFIR_INSTALL_DESTINATION_DOC}
    COMPONENT Development)

  add_custom_target(requirements ALL DEPENDS requirements_specification.pdf requirements_specification.html)
else()
  message(STATUS "dblatex executable not found, not generating requirements_specification.pdf")

  add_custom_target(requirements ALL DEPENDS requirements_specification.html)
endif()

