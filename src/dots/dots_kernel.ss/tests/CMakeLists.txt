cmake_minimum_required(VERSION 2.8)

project(dots_kernel_test CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FIND_PACKAGE(Boost COMPONENTS filesystem thread system)

add_executable(multiple_accesses multiple_accesses.cpp)
add_executable(multiple_serialization multiple_serialization.cpp)
add_executable(repository_compare repository_compare.cpp
                ../src/dots_shm_repository.h ../src/dots_shm_repository.cpp)

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES(multiple_accesses dots_kernel ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(multiple_serialization dots_kernel ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(repository_compare lluf_id lluf_utils dots_internal ${Boost_LIBRARIES} rt)
ENDIF()

enable_testing()

add_test(multiple_init multiple_accesses)
add_test(multiple_accesses_after_init multiple_accesses sleep)
add_test(multiple_serialization multiple_serialization)
add_test(repository_compare repository_compare ../../../dose/dose_dou.ss)

SET_SAFIR_TEST_PROPERTIES(multiple_init 120)
SET_SAFIR_TEST_PROPERTIES(multiple_accesses_after_init 120)
SET_SAFIR_TEST_PROPERTIES(multiple_serialization 120)
SET_SAFIR_TEST_PROPERTIES(repository_compare 120)
