#These tests are slow and system-level, so they are not run as part of ctest.
#Instead the driver scripts and their config are installed into the TestSuite
#component and run against an installed package, just like the dose tests (see
#src/tests/dose_test.ss). The production binaries the tests drive (safir_control,
#dose_main, dope_main, safir_show_config, safir_web) are all found on the PATH in
#bin. The DoseTest dou types are provided by the installed DoseTest test-suite
#library (see src/tests/dose_test_dou.ss).
foreach(script
    run_light_nodes_clear_state_tests
    run_light_nodes_keep_state_tests
    run_light_nodes_smart_sync_tests)
  configure_file(${script}.py.in "${CMAKE_CURRENT_BINARY_DIR}/${script}.py" @ONLY)
  SAFIR_INSTALL_PYTHON(SCRIPT ${CMAKE_CURRENT_BINARY_DIR}/${script}.py
    COMPONENT TestSuite)
endforeach()

INSTALL(DIRECTORY test_config
  DESTINATION ${SAFIR_INSTALL_DESTINATION_TEST_DATA}/light_nodes
  COMPONENT TestSuite)
