#This test is slow and system-level, so it is not run as part of ctest. Instead
#the driver script and its config are installed into the TestSuite component and
#run against an installed package, just like the dope odbc backend test. The
#production binaries the test drives (safir_control, dose_main, dope_main,
#safir_show_config, dope_bin2xml) are all found on the PATH in bin, as is the
#test-only binary entity_owner that is installed by the parent CMakeLists. The
#DopeTest dou types are provided by the installed DopeTest test-suite library
#(see src/dope/dope_test_dou.ss).
configure_file(run_dope_file_backend_test.py.in
  "${CMAKE_CURRENT_BINARY_DIR}/run_dope_file_backend_test.py" @ONLY)
SAFIR_INSTALL_PYTHON(SCRIPT ${CMAKE_CURRENT_BINARY_DIR}/run_dope_file_backend_test.py
  COMPONENT TestSuite)

INSTALL(DIRECTORY test_config
  DESTINATION ${SAFIR_INSTALL_DESTINATION_TEST_DATA}/dope/file_backend
  COMPONENT TestSuite)
