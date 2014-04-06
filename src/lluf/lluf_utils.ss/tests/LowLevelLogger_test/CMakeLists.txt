ADD_EXECUTABLE(lll_test lll_test.cpp)

TARGET_LINK_LIBRARIES(lll_test lluf_utils ${Boost_LIBRARIES})

add_dependencies(lll_test logger_control) #logger control is needed for the test

ADD_TEST(NAME LowLevelLogger 
  COMMAND ${PYTHON_EXECUTABLE} 
  ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --test-exe $<TARGET_FILE:lll_test>
  --control-exe $<TARGET_FILE:logger_control>
  --config-dir ${CMAKE_CURRENT_SOURCE_DIR}/configs)

