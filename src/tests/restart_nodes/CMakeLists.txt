#These tests are slow and system-level, so they are not run as part of ctest.
#Instead the driver script and its config are installed into the TestSuite
#component and run against an installed package, just like the dose tests (see
#src/tests/dose_test.ss). The production binaries the test drives (safir_control,
#dose_main, dope_main, safir_show_config) are all found on the PATH in bin, as are
#the test-only binaries WaitingStatesOwner and RequestSender that we install below.
#The DoseTest dou types are provided by the installed DoseTest test-suite library
#(see src/tests/dose_test_dou.ss).
add_executable(WaitingStatesOwner Owner.cpp)
add_executable(RequestSender RequestSender.cpp)

target_link_libraries(WaitingStatesOwner
  PRIVATE
  dose_cpp
  safir_generated-DoseTest-cpp
  Boost::program_options
  Boost::thread)

target_link_libraries(RequestSender
  PRIVATE
  dose_cpp
  safir_generated-DoseTest-cpp
  Boost::program_options)

SAFIR_INSTALL(TARGETS WaitingStatesOwner RequestSender COMPONENT TestSuite)

if (MSVC)
  if ((CMAKE_BUILD_TYPE STREQUAL "RelWithDebInfo") OR
      (CMAKE_BUILD_TYPE STREQUAL "Release"))
    set(RESTART_NODES_NUM_CLIENTS 4)
  else()
    set(RESTART_NODES_NUM_CLIENTS 2)
  endif()
else()
  set(RESTART_NODES_NUM_CLIENTS 10)
endif()

configure_file(run_restart_nodes_tests.py.in
  "${CMAKE_CURRENT_BINARY_DIR}/run_restart_nodes_tests.py" @ONLY)
SAFIR_INSTALL_PYTHON(SCRIPT ${CMAKE_CURRENT_BINARY_DIR}/run_restart_nodes_tests.py
  COMPONENT TestSuite)

INSTALL(DIRECTORY test_config
  DESTINATION ${SAFIR_INSTALL_DESTINATION_TEST_DATA}/restart_nodes
  COMPONENT TestSuite)
