add_executable(tracer_backdoor_sender sender.cpp)

target_link_libraries(tracer_backdoor_sender PRIVATE
  swre_interface_cpp
  ${Boost_ATOMIC_LIBRARY})

get_property(SAFIR_GENERATED_PATHS GLOBAL PROPERTY SAFIR_GENERATED_PATHS)

ADD_TEST(NAME tracer_backdoor
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --sender $<TARGET_FILE:tracer_backdoor_sender>
  --backdoor $<TARGET_FILE:bd>
  --safir-control $<TARGET_FILE:safir_control>
  --dose-main $<TARGET_FILE:dose_main>
  --dope-main $<TARGET_FILE:signal_persistence_ready>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  --safir-generated-paths "${SAFIR_GENERATED_PATHS}")

SET_SAFIR_TEST_PROPERTIES(TEST tracer_backdoor TIMEOUT 240
                          CONFIG_OVERRIDE "${CMAKE_CURRENT_SOURCE_DIR}/test_config")
