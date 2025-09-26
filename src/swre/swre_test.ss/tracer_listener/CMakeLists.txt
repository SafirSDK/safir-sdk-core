ADD_TEST(NAME tracer_listener
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --listener $<TARGET_FILE:safir_tracer_listener>)

SET_SAFIR_TEST_PROPERTIES(TEST tracer_listener TIMEOUT 240)
