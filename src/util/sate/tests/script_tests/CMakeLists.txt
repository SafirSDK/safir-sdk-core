ADD_TEST(NAME sate_script
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --safir-control $<TARGET_FILE:safir_control>
  --dose_main $<TARGET_FILE:dose_main>
  --dope_main $<TARGET_FILE:dope_main>
  --sate $<TARGET_FILE:sate>
  --script1 ${CMAKE_CURRENT_SOURCE_DIR}/scripts/script1.json
  --script2 ${CMAKE_CURRENT_SOURCE_DIR}/scripts/script2.json
  --safir-show-config $<TARGET_FILE:safir_show_config>
  )

SET_SAFIR_TEST_PROPERTIES(TEST sate_script
  TIMEOUT 600
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)

