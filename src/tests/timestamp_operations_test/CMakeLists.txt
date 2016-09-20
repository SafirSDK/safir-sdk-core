if (TEST_AGAINST_CORE_5)
  cmake_minimum_required(VERSION 2.8)

  project(timestamp_operations_test CXX)

  INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

  add_definitions(-DTEST_AGAINST_CORE_5)

  ADD_EXECUTABLE(timestamp_operations_test timestamp_operations_test.cpp)

  FIND_PACKAGE(Boost COMPONENTS thread system unit_test_framework)

  TARGET_LINK_LIBRARIES(timestamp_operations_test
    swre_interface_cpp
    dose_internal
    dose_cpp dots_generated-Safir-cpp dots_generated-DoseTest-cpp dots_cpp ${Boost_LIBRARIES}
    ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})
else()

  ADD_EXECUTABLE(timestamp_operations_test timestamp_operations_test.cpp)

  TARGET_LINK_LIBRARIES(timestamp_operations_test PRIVATE
    dose_internal
    safir_generated-DoseTest-cpp
    ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})

  ADD_TEST(NAME timestamp_operations_test COMMAND timestamp_operations_test)

  SET_SAFIR_TEST_PROPERTIES(TEST timestamp_operations_test
    CONFIG_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/test_config)
endif()
