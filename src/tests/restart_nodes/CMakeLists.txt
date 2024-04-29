#if dobexplorer is not built we cant run this test.
if (NOT TARGET dobexplorer)
  return()
endif()

add_executable(WaitingStatesOwner Owner.cpp)
add_executable(RequestSender RequestSender.cpp)

target_link_libraries(WaitingStatesOwner
  PRIVATE
  dose_cpp
  safir_generated-DoseTest-cpp
  Boost::program_options
  Boost::thread)

target_link_libraries(RequestSender
  PRIVATE
  dose_cpp
  safir_generated-DoseTest-cpp
  Boost::program_options)

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

ADD_TEST(NAME restart_nodes_with_entity_updates_only
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --owner $<TARGET_FILE:WaitingStatesOwner>
  --sender $<TARGET_FILE:RequestSender>
  --dobexplorer $<TARGET_FILE:dobexplorer>
  --safir-control $<TARGET_FILE:safir_control>
  --dose_main $<TARGET_FILE:dose_main>
  --dope_main $<TARGET_FILE:dope_main>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  --clients ${num_clients}
  --entity-updates)

SET_SAFIR_TEST_PROPERTIES(TEST restart_nodes_with_entity_updates_only
  TIMEOUT 3000
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)

ADD_TEST(NAME restart_nodes_with_entity_requests_only
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --owner $<TARGET_FILE:WaitingStatesOwner>
  --sender $<TARGET_FILE:RequestSender>
  --dobexplorer $<TARGET_FILE:dobexplorer>
  --safir-control $<TARGET_FILE:safir_control>
  --dose_main $<TARGET_FILE:dose_main>
  --dope_main $<TARGET_FILE:dope_main>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  --clients ${num_clients}
  --handlers 2
  --entity-requests)

SET_SAFIR_TEST_PROPERTIES(TEST restart_nodes_with_entity_requests_only
  TIMEOUT 6000
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)

ADD_TEST(NAME restart_nodes_with_entity_requests_and_updates
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --owner $<TARGET_FILE:WaitingStatesOwner>
  --sender $<TARGET_FILE:RequestSender>
  --dobexplorer $<TARGET_FILE:dobexplorer>
  --safir-control $<TARGET_FILE:safir_control>
  --dose_main $<TARGET_FILE:dose_main>
  --dope_main $<TARGET_FILE:dope_main>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  --clients ${num_clients}
  --handlers 2
  --entity-requests
  --entity-updates
  --update-period 25)

SET_SAFIR_TEST_PROPERTIES(TEST restart_nodes_with_entity_requests_and_updates
  TIMEOUT 6000
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)

