add_executable(tracer_backdoor_sender sender.cpp)

if (NOT MSVC)
  target_link_libraries(tracer_backdoor_sender dots_generated-Safir-cpp swre_interface_cpp dose_cpp dots_cpp ${Boost_LIBRARIES})
endif()

ADD_TEST(tracer_backdoor ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(tracer_backdoor 120)
