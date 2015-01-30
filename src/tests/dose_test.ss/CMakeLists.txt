ADD_SUBDIRECTORY(util)
ADD_SUBDIRECTORY(cpp)
ADD_SUBDIRECTORY(dotnet)


ADD_SUBDIRECTORY(java)

#if (SAFIR_ADA_SUPPORT)
# ADD_SUBDIRECTORY(ada)
#endif ()



ADD_SUBDIRECTORY(Sequencer)
ADD_SUBDIRECTORY(StressTests)

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
