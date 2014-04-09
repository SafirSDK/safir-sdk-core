add_executable(swreport_sender_cpp sender.cpp)

if (NOT MSVC)
  target_link_libraries(swreport_sender_cpp dots_cpp dose_cpp swre_interface_cpp ${Boost_LIBRARIES})
endif()

ADD_TEST(swreport_cpp ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(TEST swreport_cpp TIMEOUT 120)
