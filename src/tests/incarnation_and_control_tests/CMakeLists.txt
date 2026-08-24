#These tests are slow and system-level, so they are not run as part of ctest.
#Instead the driver script and its config are installed into the TestSuite
#component and run against an installed package, just like the dose tests (see
#src/tests/dose_test.ss). The production binaries the test drives (safir_control,
#dose_main, dope_main, system_picture_listener, safir_control_cli,
#safir_show_config) are all found on the PATH in bin.
configure_file(run_incarnation_and_control_tests.py.in
  "${CMAKE_CURRENT_BINARY_DIR}/run_incarnation_and_control_tests.py" @ONLY)

SAFIR_INSTALL_PYTHON(SCRIPT ${CMAKE_CURRENT_BINARY_DIR}/run_incarnation_and_control_tests.py
  COMPONENT TestSuite)

INSTALL(DIRECTORY test_config
  DESTINATION ${SAFIR_INSTALL_DESTINATION_TEST_DATA}/incarnation_and_control
  COMPONENT TestSuite)
