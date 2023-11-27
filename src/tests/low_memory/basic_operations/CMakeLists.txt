#if dobexplorer is not built we cant run this test.
if (NOT TARGET dobexplorer)
  return()
endif()

add_executable(basic_operations basic_operations.cpp)

target_link_libraries(basic_operations PRIVATE
  dose_cpp
  dose_internal
  safir_generated-DoseTest-cpp
  Boost::thread
)

ADD_TEST(NAME lowmem_basic_operations
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --basic-operations $<TARGET_FILE:basic_operations>
  --safir-control $<TARGET_FILE:safir_control>
  --dose_main $<TARGET_FILE:dose_main>
  --dope_main $<TARGET_FILE:dope_main>
  --dobexplorer $<TARGET_FILE:dobexplorer>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  )

SET_SAFIR_TEST_PROPERTIES(TEST lowmem_basic_operations
  TIMEOUT 240
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)

