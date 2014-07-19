if (UNIX)
  set(DEFAULT_LOCK_FILE_DIRECTORY /var/run/safir_sdk_core/locks/)
  set(DEFAULT_CRASH_DUMP_DIRECTORY /var/run/safir_sdk_core/dump/)
  set(DEFAULT_LOG_DIRECTORY /var/log/safir_sdk_core/)
else()
  set(DEFAULT_LOCK_FILE_DIRECTORY @{CSIDL_COMMON_APPDATA}/safir_sdk_core/locks/)
  set(DEFAULT_CRASH_DUMP_DIRECTORY @{CSIDL_COMMON_APPDATA}/safir_sdk_core/dump/)
  set(DEFAULT_LOG_DIRECTORY @{CSIDL_COMMON_APPDATA}/safir_sdk_core/log)
endif()

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
