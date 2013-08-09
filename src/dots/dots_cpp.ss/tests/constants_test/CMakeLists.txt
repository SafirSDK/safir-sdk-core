cmake_minimum_required(VERSION 2.8)

project(constants_tests CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

ADD_EXECUTABLE(constants_test constants_test.cpp)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(constants_test dots_cpp)
endif()

ENABLE_TESTING()
ADD_TEST(constants_test constants_test)

FIND_SOURCE_ROOT(SOURCE_ROOT)
SET_TESTS_PROPERTIES(constants_test PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")
