ADD_EXECUTABLE(system_picture_component_test_node system_picture_component_test_node.cpp)

TARGET_LINK_LIBRARIES(system_picture_component_test_node PRIVATE
  system_picture
  lluf_crash_reporter
  lluf_internal
  lluf_utils
  lluf_id
  Boost::program_options)

ADD_TEST(NAME system_picture_component_test
  COMMAND ${Python3_EXECUTABLE}
  ${CMAKE_CURRENT_SOURCE_DIR}/run_system_picture_component_test.py
     --exe $<TARGET_FILE:system_picture_component_test_node>)

SET_SAFIR_TEST_PROPERTIES(TEST system_picture_component_test TIMEOUT 3600) #Suuuuper slow tests.

