#This test is slow and system-level, so it is not run as part of ctest. Instead
#the driver script and its config are installed into the TestSuite component and
#run against an installed package, just like the dose tests (see
#src/tests/dose_test.ss). The production binaries the test drives (safir_control,
#dose_main, dope_main, safir_show_config) are all found on the PATH in bin, as is
#the test-only binary basic_operations that we install below. The DoseTest dou
#types are provided by the installed DoseTest test-suite library (see
#src/tests/dose_test_dou.ss).
add_executable(basic_operations basic_operations.cpp)

target_link_libraries(basic_operations PRIVATE
  dose_cpp
  dose_internal
  safir_generated-DoseTest-cpp
  Boost::thread
)

SAFIR_INSTALL(TARGETS basic_operations COMPONENT TestSuite)

configure_file(run_lowmem_basic_operations_tests.py.in
  "${CMAKE_CURRENT_BINARY_DIR}/run_lowmem_basic_operations_tests.py" @ONLY)
SAFIR_INSTALL_PYTHON(SCRIPT ${CMAKE_CURRENT_BINARY_DIR}/run_lowmem_basic_operations_tests.py
  COMPONENT TestSuite)

INSTALL(DIRECTORY test_config
  DESTINATION ${SAFIR_INSTALL_DESTINATION_TEST_DATA}/low_memory/basic_operations
  COMPONENT TestSuite)
