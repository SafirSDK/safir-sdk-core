add_executable(tracer_sender_cpp_no_flush sender.cpp)

target_link_libraries(tracer_sender_cpp_no_flush PRIVATE
  swre_interface_cpp)

ADD_TEST(NAME tracer_cpp_no_flush
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --binary $<TARGET_FILE:tracer_sender_cpp_no_flush>
  --safir-show-config $<TARGET_FILE:safir_show_config>)


SET_SAFIR_TEST_PROPERTIES(TEST tracer_cpp_no_flush
  TIMEOUT 30)
