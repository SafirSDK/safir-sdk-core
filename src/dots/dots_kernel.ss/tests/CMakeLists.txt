cmake_minimum_required(VERSION 2.8)

project(dots_kernel_test CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FIND_PACKAGE(Boost COMPONENTS thread)
add_executable(multiple_accesses multiple_accesses.cpp)
add_executable(multiple_serialization multiple_serialization.cpp)

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES(multiple_accesses dots_kernel ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(multiple_serialization dots_kernel ${Boost_LIBRARIES})
ENDIF()

enable_testing()

add_test(multiple_init multiple_accesses)
add_test(multiple_accesses_after_init multiple_accesses sleep)
add_test(multiple_serialization multiple_serialization)
