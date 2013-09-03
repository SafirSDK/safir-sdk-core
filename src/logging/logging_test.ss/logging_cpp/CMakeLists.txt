add_executable(log_sender sender.cpp)

if (NOT MSVC)
  target_link_libraries(log_sender logging_cpp)
endif()

ADD_TEST(logging_cpp ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_TESTS_PROPERTIES(logging_cpp PROPERTIES TIMEOUT 120)
SET_PROPERTY(TEST logging_cpp
  PROPERTY ENVIRONMENT 
  "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config"
  "PYTHONPATH=$ENV{PYTHONPATH}:${SOURCE_ROOT}/src/tests/test_support/python")
