ADD_TEST(NAME crash_dump_monitor_test 
  COMMAND ${PYTHON_EXECUTABLE} 
  ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py 
  --show-config $<TARGET_FILE:safir_show_config>
  --dump-monitor $<TARGET_FILE:crash_dump_monitor>
  --config-dir ${CMAKE_CURRENT_SOURCE_DIR}/testconf)

SET_SAFIR_TEST_PROPERTIES(crash_dump_monitor_test 120)




