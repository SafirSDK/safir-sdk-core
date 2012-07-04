cmake_minimum_required(VERSION 2.4)

project(dots_id_test CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

ADD_EXECUTABLE(dots_id_test dots_id_test.cpp)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(dots_id_test dots_id)
endif()

ENABLE_TESTING()
ADD_TEST(dots_id dots_id_test)
