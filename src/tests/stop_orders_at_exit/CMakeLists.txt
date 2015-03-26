add_executable(stoppee stoppee.cpp)

target_link_libraries(stoppee PRIVATE
  dose_cpp)

get_property(SAFIR_GENERATED_PATHS GLOBAL PROPERTY SAFIR_GENERATED_PATHS)

ADD_TEST(NAME stop_orders_at_exit
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --stoppee $<TARGET_FILE:stoppee>
  --safir-control $<TARGET_FILE:safir_control>
  --dose_main $<TARGET_FILE:dose_main>
  --dope_main $<TARGET_FILE:dope_main>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  --safir-generated-paths "${SAFIR_GENERATED_PATHS}")

SET_SAFIR_TEST_PROPERTIES(TEST stop_orders_at_exit
  TIMEOUT 240
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)

