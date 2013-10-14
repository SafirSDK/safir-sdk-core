cmake_minimum_required(VERSION 2.8)

project(constants_tests CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

ADD_EXECUTABLE(constants_test constants_test.cpp)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(constants_test dots_cpp)
endif()

ENABLE_TESTING()
ADD_TEST(constants_test constants_test)

SET_SAFIR_TEST_PROPERTIES(constants_test)
