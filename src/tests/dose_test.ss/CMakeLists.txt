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
  set(DEFAULT_DOU_DIRECTORY "${CMAKE_INSTALL_PREFIX}/${SAFIR_INSTALL_DESTINATION_DOU_BASE}")
  set(DEFAULT_JAVA_SEARCH_PATH "${CMAKE_INSTALL_PREFIX}/${SAFIR_INSTALL_DESTINATION_JAR}")
  set(TEST_DATA_DIRECTORY "${CMAKE_INSTALL_PREFIX}/${SAFIR_INSTALL_DESTINATION_TEST_DATA}")
else()
  set(DEFAULT_DOU_DIRECTORY "C:/Program Files/Safir SDK Core/${SAFIR_INSTALL_DESTINATION_DOU_BASE}")
  set(DEFAULT_JAVA_SEARCH_PATH "C:/Program Files/Safir SDK Core/${SAFIR_INSTALL_DESTINATION_JAR}")
  set(TEST_DATA_DIRECTORY "C:/Program Files/Safir SDK Core/${SAFIR_INSTALL_DESTINATION_TEST_DATA}")
endif()



#The reason for the C:/Program... stuff above is that the CMAKE_INSTALL_PREFIX contains
#"safir_sdk_core" instead of "Safir SDK Core", which is not where we install by default.
#Maybe it is possible to find some better way of doing this...
#TODO: this is in src/config/CMakeLists.txt as well. Fix somehow?

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
