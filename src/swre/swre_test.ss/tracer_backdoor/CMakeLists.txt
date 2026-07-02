#This test is slow and system-level, so it is not run as part of ctest. Instead
#the driver script and its config are installed into the TestSuite component and
#run against an installed package, just like the dose tests. The production
#binaries the test drives (safir_control, dose_main, safir_show_config, safir_web,
#safir_status and the bd backdoor tool) are all found on the PATH in bin, as are
#the test-only binaries tracer_backdoor_sender (installed below) and
#signal_persistence_ready (installed by src/dose/dose_main.ss/tests, used here as a
#stand-in for dope_main). The types used are Core dou types, provided by the
#installed Core dou library.
add_executable(tracer_backdoor_sender sender.cpp)

target_link_libraries(tracer_backdoor_sender PRIVATE
  swre_interface_cpp)

SAFIR_INSTALL(TARGETS tracer_backdoor_sender COMPONENT TestSuite)

configure_file(run_tracer_backdoor_tests.py.in
  "${CMAKE_CURRENT_BINARY_DIR}/run_tracer_backdoor_tests.py" @ONLY)
SAFIR_INSTALL_PYTHON(SCRIPT ${CMAKE_CURRENT_BINARY_DIR}/run_tracer_backdoor_tests.py
  COMPONENT TestSuite)

INSTALL(DIRECTORY test_config
  DESTINATION ${SAFIR_INSTALL_DESTINATION_TEST_DATA}/swre/tracer_backdoor
  COMPONENT TestSuite)
