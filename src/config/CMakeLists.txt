if (UNIX)
  set(DEFAULT_LOCK_FILE_DIRECTORY /var/run/safir-sdk-core/locks/)
  set(DEFAULT_CRASH_DUMP_DIRECTORY /var/lib/safir-sdk-core/dump/)
  set(DEFAULT_LOG_DIRECTORY /var/log/safir-sdk-core/)
  set(DEFAULT_DOU_DIRECTORY "${CMAKE_INSTALL_PREFIX}/${SAFIR_INSTALL_DESTINATION_DOU_BASE}")
  set(DEFAULT_JAVA_SEARCH_PATH "${CMAKE_INSTALL_PREFIX}/${SAFIR_INSTALL_DESTINATION_JAR}")

else()
  set(DEFAULT_LOCK_FILE_DIRECTORY @{CSIDL_COMMON_APPDATA}/safir-sdk-core/locks/)
  set(DEFAULT_CRASH_DUMP_DIRECTORY @{CSIDL_COMMON_APPDATA}/safir-sdk-core/dump/)
  set(DEFAULT_LOG_DIRECTORY @{CSIDL_COMMON_APPDATA}/safir-sdk-core/log)
  set(DEFAULT_DOU_DIRECTORY "C:/Program Files/Safir SDK Core/${SAFIR_INSTALL_DESTINATION_DOU_BASE}")
  set(DEFAULT_JAVA_SEARCH_PATH "C:/Program Files/Safir SDK Core/${SAFIR_INSTALL_DESTINATION_JAR}")
endif()

#The reason for the C:/Program... stuff above is that the CMAKE_INSTALL_PREFIX contains
#"safir-sdk-core" instead of "Safir SDK Core", which is not where we install by default.
#Maybe it is possible to find some better way of doing this...



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
