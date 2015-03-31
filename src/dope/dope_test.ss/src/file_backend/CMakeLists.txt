get_property(SAFIR_GENERATED_PATHS GLOBAL PROPERTY SAFIR_GENERATED_PATHS)

ADD_TEST(NAME dope_file_backend_test 
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_file_backend_test.py
  --safir-show-config $<TARGET_FILE:safir_show_config>
  --safir-control $<TARGET_FILE:safir_control>
  --dose-main $<TARGET_FILE:dose_main>
  --dope-main $<TARGET_FILE:dope_main>
  --entity-owner $<TARGET_FILE:entity_owner>
  --dope-bin2xml $<TARGET_FILE:dope_bin2xml>
  --safir-generated-paths "${SAFIR_GENERATED_PATHS}")


#Long timeouts are needed on slow machines.
SET_SAFIR_TEST_PROPERTIES(TEST dope_file_backend_test 
  TIMEOUT 3600
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)
