add_executable(log_sender sender.cpp)

target_link_libraries(log_sender logging_cpp)

ADD_TEST(NAME logging_cpp 
  COMMAND ${PYTHON_EXECUTABLE} 
  ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --sender $<TARGET_FILE:log_sender>
  --safir-show-config $<TARGET_FILE:safir_show_config>)

SET_SAFIR_TEST_PROPERTIES(logging_cpp 120)
