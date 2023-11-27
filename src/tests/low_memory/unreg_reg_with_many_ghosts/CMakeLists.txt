#if dobexplorer is not built we cant run this test.
if (NOT TARGET dobexplorer)
  return()
endif()

add_executable(unreg_reg_with_many_ghosts unreg_reg_with_many_ghosts.cpp)

target_link_libraries(unreg_reg_with_many_ghosts PRIVATE
  dose_cpp
  dose_internal
  safir_generated-DoseTest-cpp
  lluf_utils
  Boost::thread
)

ADD_TEST(NAME lowmem_unreg_reg_with_many_ghosts
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --unreg_reg_with_many_ghosts $<TARGET_FILE:unreg_reg_with_many_ghosts>
  --subscriber $<TARGET_FILE:slow_entity_subscriber>
  --safir-control $<TARGET_FILE:safir_control>
  --dose_main $<TARGET_FILE:dose_main>
  --dope_main $<TARGET_FILE:dope_main>
  --dobexplorer $<TARGET_FILE:dobexplorer>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  )

SET_SAFIR_TEST_PROPERTIES(TEST lowmem_unreg_reg_with_many_ghosts
  TIMEOUT 240
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)

