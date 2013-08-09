add_executable(panic_log_sender sender.cpp)

if (NOT MSVC)
  target_link_libraries(panic_log_sender dose_cpp dots_cpp lluf_utils ${Boost_LIBRARIES})
endif()

ADD_TEST(panic_log_reception ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_TESTS_PROPERTIES(panic_log_reception PROPERTIES TIMEOUT 120)
SET_TESTS_PROPERTIES(panic_log_reception PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")
