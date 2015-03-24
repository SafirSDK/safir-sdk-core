ADD_EXECUTABLE(lll_test lll_test.cpp)

TARGET_LINK_LIBRARIES(lll_test PRIVATE
  lluf_utils)

ADD_TEST(NAME LowLevelLogger
  COMMAND ${PYTHON_EXECUTABLE}
  ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --test-exe $<TARGET_FILE:lll_test>
  --config-dir ${CMAKE_CURRENT_SOURCE_DIR}/configs)


SET_SAFIR_TEST_PROPERTIES(TEST LowLevelLogger TIMEOUT 600)
