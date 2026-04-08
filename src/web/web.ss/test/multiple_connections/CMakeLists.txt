ADD_TEST(NAME websocket_multiple_connections
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --safir-control $<TARGET_FILE:safir_control>
  --dose_main $<TARGET_FILE:dose_main>
  --dope_main $<TARGET_FILE:dope_main>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  --safir_web $<TARGET_FILE:safir_web>)

SET_SAFIR_TEST_PROPERTIES(TEST websocket_multiple_connections
  TIMEOUT 120
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/../test_client/test_config)
