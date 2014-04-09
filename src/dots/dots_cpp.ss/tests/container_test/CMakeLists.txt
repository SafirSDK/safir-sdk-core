cmake_minimum_required(VERSION 2.8)

project(dots_cpp_tests CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

if (UNIX)
  SET(CMAKE_CXX_FLAGS "-Wno-unused-value")
endif()

ADD_EXECUTABLE(container_test container_test.cpp)


if (NOT MSVC)
  TARGET_LINK_LIBRARIES(container_test dots_generated-DotsTest-cpp dots_generated-Safir-cpp dots_cpp)
endif()

ENABLE_TESTING()
ADD_TEST(container_test container_test)

SET_SAFIR_TEST_PROPERTIES(TEST container_test)
