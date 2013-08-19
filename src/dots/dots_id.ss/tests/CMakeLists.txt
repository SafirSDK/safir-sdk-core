cmake_minimum_required(VERSION 2.8)

project(dots_id_test CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

ADD_EXECUTABLE(dots_id_test dots_id_test.cpp)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(dots_id_test dots_id)
endif()

ENABLE_TESTING()
ADD_TEST(dots_id dots_id_test)

FIND_SOURCE_ROOT(SOURCE_ROOT)
SET_TESTS_PROPERTIES(dots_id PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")
