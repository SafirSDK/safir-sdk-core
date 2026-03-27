add_executable(tracer_syslog_forward_sender sender.cpp)

target_link_libraries(tracer_syslog_forward_sender PRIVATE
  swre_interface_cpp
  lluf_internal)

ADD_TEST(NAME tracer_syslog_forward
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --binary $<TARGET_FILE:tracer_syslog_forward_sender>)

SET_SAFIR_TEST_PROPERTIES(TEST tracer_syslog_forward)
