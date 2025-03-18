ADD_EXECUTABLE(timestamp_operations_test timestamp_operations_test.cpp)

TARGET_LINK_LIBRARIES(timestamp_operations_test PRIVATE
  dose_internal
  lluf_internal
  safir_generated-DoseTest-cpp
  Boost::unit_test_framework)

ADD_TEST(NAME timestamp_operations_test COMMAND timestamp_operations_test)

SET_SAFIR_TEST_PROPERTIES(TEST timestamp_operations_test
  CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)

