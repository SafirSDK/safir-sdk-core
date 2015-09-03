get_property(SAFIR_GENERATED_PATHS GLOBAL PROPERTY SAFIR_GENERATED_PATHS)

ADD_TEST(NAME Incarnation_And_Control_Tests
  COMMAND ${PYTHON_EXECUTABLE}
  ${CMAKE_CURRENT_SOURCE_DIR}/incarnation_and_control_tests.py
  --safir-control $<TARGET_FILE:safir_control>
  --dose_main $<TARGET_FILE:dose_main>
  --dope_main $<TARGET_FILE:dope_main>
  --system_picture_listener $<TARGET_FILE:system_picture_listener>
  --safir_control_cli $<TARGET_FILE:safir_control_cli>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  --safir-generated-paths "${SAFIR_GENERATED_PATHS}")

SET_SAFIR_TEST_PROPERTIES(TEST Incarnation_And_Control_Tests
  TIMEOUT 240
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)