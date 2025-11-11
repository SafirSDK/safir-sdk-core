if (NOT ASCIIDOCTOR_FOUND)
  message (STATUS "Asciidoctor not found, not generating user's guide")
  return()
endif()

#add_asciidoctor_document(users_guide.txt DIAGRAMS block_diagram.dia  example_object.dia  object_hierarchy.dia  orphan_object.dia)

set (USERS_GUIDE_TXTS users_guide.txt tracer.txt example_applications.txt websocket_api_reference.txt)

set (USERS_GUIDE_DIAS
  images/block_diagram.dia
  images/example_object.dia
  images/object_hierarchy.dia
  images/orphan_object.dia)

set (USERS_GUIDE_PNGS
  images/dispatching_sequence_diagram.png
  images/redundancy_sequence_diagram.png
  images/vehicleapp_class_diagram.png
  images/vehiclemmi_class_diagram.png
  images/dobexplorer_memory.png
  images/injection_sequence_diagram.png
  images/sate_overview.png
  images/sate_script.png
  images/vehicledb_class_diagram.png
  images/vehiclemmi_create.png
  images/dobexplorer_nodes.png
  images/messages_sequence_diagram.png
  images/separation_tenet.png
  images/vehicle_deployment.png
  images/vehiclemmi_list.png
  images/dobmake.png
  images/node_control.png
  images/services_sequence_diagram.png
  images/vehiclemmi_calculate_speed.png
  images/vehiclemmi_update.png
  images/entities_sequence_diagram.png
  images/persistence_sequence_diagram.png
  images/tool_launcher.png
  images/vehiclemmi_category_info.png
)

foreach (dia IN LISTS USERS_GUIDE_DIAS)
  string(REPLACE .dia .eps dia_eps ${dia})
  string(REPLACE .dia .png dia_png ${dia})

  add_custom_command(
    OUTPUT ${dia_png}
    COMMAND ${DIA_EXECUTABLE} --export=${dia_png} -t png ${CMAKE_CURRENT_SOURCE_DIR}/${dia}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${dia}
    COMMENT "Rendering ${dia} to ${dia_png}")

  add_custom_command(
    OUTPUT ${dia_eps}
    COMMAND ${DIA_EXECUTABLE} --export=${dia_eps} -t eps ${CMAKE_CURRENT_SOURCE_DIR}/${dia}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${dia}
    COMMENT "Rendering ${dia} to ${dia_eps}")

  list(APPEND USERS_GUIDE_RENDERED_PNGS ${dia_png})
  list(APPEND USERS_GUIDE_RENDERED_EPS ${dia_eps})
endforeach()

#if we're doing an out of source build we copy all images, just to make it possible
#to open the html file without installing
if (NOT "${CMAKE_CURRENT_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_BINARY_DIR}")
  foreach (png IN LISTS USERS_GUIDE_PNGS)
    file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/${png} DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/images/)
  endforeach()
endif()

add_custom_command(
  OUTPUT users_guide.html
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/../logo/renders/logotype256.png images/
  COMMAND ${ASCIIDOCTOR_EXECUTABLE}  -b html5
          -a stylesheet=safir-stylesheet.css
          -a SAFIR_VERSION=${SAFIR_VERSION_STRING}
          -D ${CMAKE_CURRENT_BINARY_DIR}
          ${CMAKE_CURRENT_SOURCE_DIR}/users_guide.txt
  DEPENDS ${USERS_GUIDE_TXTS} ${USERS_GUIDE_PNGS} ${USERS_GUIDE_RENDERED_PNGS} safir-stylesheet.css
  COMMENT "Generating User's Guide HTML")

INSTALL(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/images
  DESTINATION ${SAFIR_INSTALL_DESTINATION_DOC}
  COMPONENT Development
  FILES_MATCHING PATTERN "*.png")

INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/users_guide.html
  DESTINATION ${SAFIR_INSTALL_DESTINATION_DOC}
  COMPONENT Development)


FIND_PROGRAM(DBLATEX_EXECUTABLE NAMES dblatex)

if(DBLATEX_EXECUTABLE)
  add_custom_command(
    OUTPUT users_guide.pdf
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/../logo/renders/logotype1024.png images/
    COMMAND ${ASCIIDOCTOR_EXECUTABLE}  -b docbook
            -a SAFIR_VERSION=${SAFIR_VERSION_STRING}
            -D ${CMAKE_CURRENT_BINARY_DIR}
            ${CMAKE_CURRENT_SOURCE_DIR}/users_guide.txt
    COMMAND ${DBLATEX_EXECUTABLE}
            -p ${CMAKE_CURRENT_SOURCE_DIR}/asciidoc-dblatex.xsl
            -s ${CMAKE_CURRENT_SOURCE_DIR}/custom-coverpage-dblatex.sty
            ${CMAKE_CURRENT_BINARY_DIR}/users_guide.xml
    DEPENDS ${USERS_GUIDE_TXTS} ${USERS_GUIDE_PNGS} ${USERS_GUIDE_RENDERED_EPS} custom-coverpage-dblatex.sty asciidoc-dblatex.xsl
    COMMENT "Generating User's Guide PDF")

  INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/users_guide.pdf
    DESTINATION ${SAFIR_INSTALL_DESTINATION_DOC}
    COMPONENT Development)

  add_custom_target(users_guide ALL DEPENDS users_guide.pdf users_guide.html)
else()
  message(STATUS "dblatex executable not found, not generating users_guide.pdf")

  add_custom_target(users_guide ALL DEPENDS users_guide.html)
endif()

