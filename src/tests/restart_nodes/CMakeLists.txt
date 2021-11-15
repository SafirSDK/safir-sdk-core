add_executable(WaitingStatesOwner Owner.cpp)

target_link_libraries(WaitingStatesOwner
  PRIVATE
  dose_cpp
  safir_generated-DoseTest-cpp)

get_property(SAFIR_GENERATED_PATHS GLOBAL PROPERTY SAFIR_GENERATED_PATHS)

if (MSVC)
  if ((CMAKE_BUILD_TYPE STREQUAL "RelWithDebInfo") OR
      (CMAKE_BUILD_TYPE STREQUAL "Release"))
    set(num_clients 4)
  else()
    set(num_clients 2)
  endif()
else()
  set(num_clients 10)
endif()

ADD_TEST(NAME restart_nodes
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --owner $<TARGET_FILE:WaitingStatesOwner>
  --dobexplorer $<TARGET_FILE:dobexplorer>
  --safir-control $<TARGET_FILE:safir_control>
  --dose_main $<TARGET_FILE:dose_main>
  --dope_main $<TARGET_FILE:dope_main>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  --clients ${num_clients})

SET_SAFIR_TEST_PROPERTIES(TEST restart_nodes
  TIMEOUT 3000
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)

