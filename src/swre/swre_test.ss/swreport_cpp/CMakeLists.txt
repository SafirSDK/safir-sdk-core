add_executable(swreport_sender_cpp sender.cpp)

target_link_libraries(swreport_sender_cpp PRIVATE
  swre_interface_cpp)

ADD_TEST(NAME swreport_cpp 
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --binary $<TARGET_FILE:swreport_sender_cpp>
  --safir-show-config $<TARGET_FILE:safir_show_config>)

SET_SAFIR_TEST_PROPERTIES(TEST swreport_cpp TIMEOUT 120)
