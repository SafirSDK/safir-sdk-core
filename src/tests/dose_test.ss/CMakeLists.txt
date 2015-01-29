ADD_SUBDIRECTORY(util)
ADD_SUBDIRECTORY(cpp)
ADD_SUBDIRECTORY(dotnet)


ADD_SUBDIRECTORY(java)

#if (SAFIR_ADA_SUPPORT)
# ADD_SUBDIRECTORY(ada)
#endif ()



ADD_SUBDIRECTORY(Sequencer)
ADD_SUBDIRECTORY(StressTests)

if (UNIX)
  set(DEFAULT_DOU_SEARCH_PATH "${CMAKE_INSTALL_PREFIX}/${SAFIR_INSTALL_DESTINATION_DOU_BASE}")
  set(DEFAULT_JAVA_SEARCH_PATH "${CMAKE_INSTALL_PREFIX}/${SAFIR_INSTALL_DESTINATION_JAR}")
  set(TEST_DATA_DIRECTORY "${CMAKE_INSTALL_PREFIX}/${SAFIR_INSTALL_DESTINATION_TEST_DATA}")
else()

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
  set(TEST_DATA_DIRECTORY "${SAFIR_INSTALL_PREFIX}/${SAFIR_INSTALL_DESTINATION_TEST_DATA}")
  unset (SAFIR_INSTALL_PREFIX) #unset the path, so noone else tries to use it!
endif()

configure_file(run_dose_tests.py.in
  "${CMAKE_CURRENT_BINARY_DIR}/run_dose_tests.py" @ONLY)

SAFIR_INSTALL_PYTHON(SCRIPT ${CMAKE_CURRENT_BINARY_DIR}/run_dose_tests.py
  COMPONENT Test)

INSTALL(
  DIRECTORY
    testcases
    output_standalone
    output_multinode
    test_config
  DESTINATION ${SAFIR_INSTALL_DESTINATION_TEST_DATA}/dose
  COMPONENT Test
  )
