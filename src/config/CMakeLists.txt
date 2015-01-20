if (UNIX)
  set(DEFAULT_LOCK_FILE_DIRECTORY /var/run/safir-sdk-core/locks/)
  set(DEFAULT_CRASH_DUMP_DIRECTORY /var/lib/safir-sdk-core/dump/)
  set(DEFAULT_LOG_DIRECTORY /var/log/safir-sdk-core/)
  set(DEFAULT_DOU_SEARCH_PATH "${CMAKE_INSTALL_PREFIX}/${SAFIR_INSTALL_DESTINATION_DOU_BASE}")
  set(DEFAULT_JAVA_SEARCH_PATH "${CMAKE_INSTALL_PREFIX}/${SAFIR_INSTALL_DESTINATION_JAR}")

else()
  set(DEFAULT_LOCK_FILE_DIRECTORY @{CSIDL_COMMON_APPDATA}/safir-sdk-core/locks/)
  set(DEFAULT_CRASH_DUMP_DIRECTORY @{CSIDL_COMMON_APPDATA}/safir-sdk-core/dump/)
  set(DEFAULT_LOG_DIRECTORY @{CSIDL_COMMON_APPDATA}/safir-sdk-core/log)

  #Work out our default install directory.  The reason for this stuff is that the
  #CMAKE_INSTALL_PREFIX contains "safir-sdk-core" instead of "Safir SDK Core", which is
  #not where we install by default.

  message("CMAKE_INSTALL_PREFIX = ${CMAKE_INSTALL_PREFIX}")
  set(SAFIR_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}/../Safir SDK Core")
  get_filename_component(SAFIR_INSTALL_PREFIX ${SAFIR_INSTALL_PREFIX} ABSOLUTE)

  set(DEFAULT_DOU_SEARCH_PATH "${SAFIR_INSTALL_PREFIX}/${SAFIR_INSTALL_DESTINATION_DOU_BASE}")
  set(DEFAULT_JAVA_SEARCH_PATH "${SAFIR_INSTALL_PREFIX}/${SAFIR_INSTALL_DESTINATION_JAR}")
  unset (SAFIR_INSTALL_PREFIX) #unset the path, so noone else tries to use it!
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
