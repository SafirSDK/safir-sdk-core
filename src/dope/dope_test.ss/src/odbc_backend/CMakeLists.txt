configure_file(run_dope_odbc_backend_test.py.in
  "${CMAKE_CURRENT_BINARY_DIR}/run_dope_odbc_backend_test.py" @ONLY)

SAFIR_INSTALL_PYTHON(SCRIPT ${CMAKE_CURRENT_BINARY_DIR}/run_dope_odbc_backend_test.py
  COMPONENT Test)

INSTALL(
  DIRECTORY
    test_config
  DESTINATION ${SAFIR_INSTALL_DESTINATION_TEST_DATA}/dope
  COMPONENT Test
  )
