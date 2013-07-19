add_executable(swreport_systemlog_sender sender.cpp)

if (NOT MSVC)
  target_link_libraries(swreport_systemlog_sender swre_interface_cpp ${Boost_LIBRARIES})
endif()

ADD_TEST(swreport_send_legacy ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py ${CMAKE_CURRENT_SOURCE_DIR}/testconf)

set_tests_properties(swreport_send_legacy PROPERTIES TIMEOUT 120)
