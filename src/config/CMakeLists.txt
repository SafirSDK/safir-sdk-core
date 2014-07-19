configure_file(typesystem.ini.in
  "${CMAKE_CURRENT_BINARY_DIR}/typesystem.ini" @ONLY)

configure_file(locations.ini.in
  "${CMAKE_CURRENT_BINARY_DIR}/locations.ini" @ONLY)

configure_file(logging.ini.in
  "${CMAKE_CURRENT_BINARY_DIR}/logging.ini" @ONLY)

INSTALL(FILES 
  "${CMAKE_CURRENT_BINARY_DIR}/typesystem.ini"
  "${CMAKE_CURRENT_BINARY_DIR}/locations.ini"
  "${CMAKE_CURRENT_BINARY_DIR}/logging.ini"
  DESTINATION ${SAFIR_INSTALL_DESTINATION_DOC}/example_configuration
  COMPONENT Runtime)
