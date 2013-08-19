add_executable(send_something sender.cpp)

if (NOT MSVC)
  target_link_libraries(send_something dots_cpp dose_cpp swre_interface_cpp ${Boost_LIBRARIES})
endif()

ADD_TEST(stop_orders_at_exit ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_TESTS_PROPERTIES(stop_orders_at_exit PROPERTIES TIMEOUT 240)
SET_TESTS_PROPERTIES(stop_orders_at_exit PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")
