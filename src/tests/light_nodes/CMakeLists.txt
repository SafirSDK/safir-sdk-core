ADD_TEST(NAME light_nodes_clear_state
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/light_nodes_clear_state.py  
  --safir-control $<TARGET_FILE:safir_control>
  --dose_main $<TARGET_FILE:dose_main>
  --dope_main $<TARGET_FILE:dope_main>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  --safir_websocket $<TARGET_FILE:safir_websocket>)

SET_SAFIR_TEST_PROPERTIES(TEST light_nodes_clear_state
  TIMEOUT 3000
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)


ADD_TEST(NAME light_nodes_keep_state
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/light_nodes_keep_state.py  
  --safir-control $<TARGET_FILE:safir_control>
  --dose_main $<TARGET_FILE:dose_main>
  --dope_main $<TARGET_FILE:dope_main>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  --safir_websocket $<TARGET_FILE:safir_websocket>)

SET_SAFIR_TEST_PROPERTIES(TEST light_nodes_keep_state
  TIMEOUT 3000
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)
