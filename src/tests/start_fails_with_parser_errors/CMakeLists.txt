
ADD_TEST(NAME start_fails_with_parser_errors
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --safir-control $<TARGET_FILE:safir_control>
  --dose_main $<TARGET_FILE:dose_main>
  --dots_configuration_check $<TARGET_FILE:dots_configuration_check>
)

SET_SAFIR_TEST_PROPERTIES(TEST start_fails_with_parser_errors
  TIMEOUT 60
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)
