add_executable(tracer_sender_cpp sender.cpp)

if (NOT MSVC)
  target_link_libraries(tracer_sender_cpp dots_cpp dose_cpp swre_interface_cpp ${Boost_LIBRARIES})
endif()

ADD_TEST(tracer_cpp ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(TEST tracer_cpp TIMEOUT 120)
