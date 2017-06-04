
add_executable(WaitingStatesOwner Owner.cpp)

target_link_libraries(WaitingStatesOwner
  PRIVATE
  dose_cpp
  safir_generated-DoseTest-cpp
  )
  #safir_generated-WaitingStates-cpp ${SAFIR_SDK_CORE_LIBRARIES})


get_property(SAFIR_GENERATED_PATHS GLOBAL PROPERTY SAFIR_GENERATED_PATHS)

ADD_TEST(NAME restart_nodes
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --owner $<TARGET_FILE:WaitingStatesOwner>
  --dobexplorer $<TARGET_FILE:dobexplorer>
  --safir-control $<TARGET_FILE:safir_control>
  --dose_main $<TARGET_FILE:dose_main>
  --dope_main $<TARGET_FILE:dope_main>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  --safir-generated-paths "${SAFIR_GENERATED_PATHS}")

SET_SAFIR_TEST_PROPERTIES(TEST restart_nodes
  TIMEOUT 600
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)
