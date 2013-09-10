add_executable(send_something sender.cpp)

if (NOT MSVC)
  target_link_libraries(send_something dots_cpp dose_cpp swre_interface_cpp ${Boost_LIBRARIES})
endif()

ADD_TEST(stop_orders_at_exit ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(stop_orders_at_exit 240)
