add_executable(swreport_sender sender.cpp)

if (NOT MSVC)
  target_link_libraries(swreport_sender dots_cpp dose_cpp swre_interface_cpp ${Boost_LIBRARIES})
endif()

ADD_TEST(swreport_reception ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_TESTS_PROPERTIES(swreport_reception PROPERTIES TIMEOUT 120)
SET_TESTS_PROPERTIES(swreport_reception PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")
