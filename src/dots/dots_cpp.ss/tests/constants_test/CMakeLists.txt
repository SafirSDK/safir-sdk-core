cmake_minimum_required(VERSION 2.6)

project(constants_tests CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

ADD_EXECUTABLE(constants_test constants_test.cpp)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(constants_test dots_cpp)
endif()

ENABLE_TESTING()
ADD_TEST(constants_test constants_test)
