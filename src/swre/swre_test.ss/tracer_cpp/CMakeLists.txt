add_executable(tracer_sender sender.cpp)

if (NOT MSVC)
  target_link_libraries(tracer_sender dots_cpp dose_cpp swre_interface_cpp ${Boost_LIBRARIES})
endif()

ADD_TEST(tracer_cpp ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

set_tests_properties(tracer_cpp PROPERTIES TIMEOUT 120)
