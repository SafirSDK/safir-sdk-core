ADD_TEST(NAME Incarnation_And_Control_Tests
  COMMAND ${Python3_EXECUTABLE}
  ${CMAKE_CURRENT_SOURCE_DIR}/incarnation_and_control_tests.py
  --safir-control $<TARGET_FILE:safir_control>
  --dose_main $<TARGET_FILE:dose_main>
  --dope_main $<TARGET_FILE:dope_main>
  --system_picture_listener $<TARGET_FILE:system_picture_listener>
  --safir_control_cli $<TARGET_FILE:safir_control_cli>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  )

SET_SAFIR_TEST_PROPERTIES(TEST Incarnation_And_Control_Tests
  TIMEOUT 1200
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)
