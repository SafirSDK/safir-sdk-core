add_executable(log_sender sender.cpp)

target_link_libraries(log_sender PRIVATE logging_cpp)

add_dependencies(log_sender safir_show_config) #test needs safir_show_config

ADD_TEST(NAME logging_cpp 
  COMMAND ${PYTHON_EXECUTABLE} 
  ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --sender $<TARGET_FILE:log_sender>
  --safir-show-config $<TARGET_FILE:safir_show_config>)

SET_SAFIR_TEST_PROPERTIES(TEST logging_cpp TIMEOUT 120)
