ADD_EXECUTABLE(system_picture_component_test_node system_picture_component_test_node.cpp)

TARGET_LINK_LIBRARIES(system_picture_component_test_node PRIVATE
  system_picture
  lluf_crash_reporter
  lluf_internal
  lluf_utils
  lluf_id
  Boost::program_options)

#These tests are slow and system-level, so they are not run as part of ctest.
#Instead the test node binary and the driver script are installed into the
#TestSuite component and run against an installed package, just like the dose
#tests (see src/tests/dose_test.ss).
SAFIR_INSTALL(TARGETS system_picture_component_test_node COMPONENT TestSuite)

configure_file(run_system_picture_component_tests.py.in
  "${CMAKE_CURRENT_BINARY_DIR}/run_system_picture_component_tests.py" @ONLY)

SAFIR_INSTALL_PYTHON(SCRIPT ${CMAKE_CURRENT_BINARY_DIR}/run_system_picture_component_tests.py
  COMPONENT TestSuite)

INSTALL(DIRECTORY config
  DESTINATION ${SAFIR_INSTALL_DESTINATION_TEST_DATA}/system_picture
  COMPONENT TestSuite)

