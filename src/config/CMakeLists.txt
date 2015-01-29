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

  #Work out our default install directory.  There are two reasons for all this stuff:
  # 1. The default CMAKE_INSTALL_PREFIX contains "safir-sdk-core" instead of
  #    "Safir SDK Core", which is where our installer installs the files.
  # 2. When installing a 32bit SDK on a 64bit OS the installer will put our files
  #    in c:\Program Files (x86), and we have to compensate for that. To do that
  #    we try to detect if CMAKE_INSTALL_PREFIX is the default, and then use
  #    special CSIDL variables in the ini files.

  string(TOUPPER "${CMAKE_INSTALL_PREFIX}" _install_prefix_uc)

  file(TO_CMAKE_PATH "$ENV{ProgramFiles}" _program_files)
  string(TOUPPER "${_program_files}" _program_files)
  string(FIND "${_install_prefix_uc}" "${_program_files}" _program_files_found)

  file(TO_CMAKE_PATH "$ENV{ProgramW6432}" _program_files64)
  string(TOUPPER "${_program_files64}" _program_files64)
  string(FIND "${_install_prefix_uc}" "${_program_files64}" _program_files64_found)

  if ((NOT "$ENV{ProgramFiles}" STREQUAL "" AND _program_files_found EQUAL 0) OR
      (NOT "$ENV{ProgramW6432}" STREQUAL "" AND _program_files64_found EQUAL 0))
    set(SAFIR_INSTALL_PREFIX "@{CSIDL_PROGRAM_FILES}/Safir SDK Core")
  else()
    set(SAFIR_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")
  endif()

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
