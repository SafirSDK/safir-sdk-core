add_executable(swreport_systemlog_sender sender.cpp)

if (NOT MSVC)
  target_link_libraries(swreport_systemlog_sender swre_utils dots_cpp dose_cpp swre_interface_cpp ${Boost_LIBRARIES})
endif()

ADD_TEST(swreport_to_systemlog_mapping ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

set_tests_properties(swreport_to_systemlog_mapping PROPERTIES TIMEOUT 120)
