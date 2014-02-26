cmake_minimum_required(VERSION 2.8)

project(lluf_id_test CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

ADD_EXECUTABLE(lluf_id_test lluf_id_test.cpp)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(lluf_id_test lluf_id)
endif()

ENABLE_TESTING()

ADD_TEST(lluf_id lluf_id_test)

SET_SAFIR_TEST_PROPERTIES(lluf_id)
