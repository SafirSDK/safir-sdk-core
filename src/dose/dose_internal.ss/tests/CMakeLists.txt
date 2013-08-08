cmake_minimum_required(VERSION 2.8)

project(dose_internal_test CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FIND_PACKAGE(Boost COMPONENTS system thread)

FIND_SOURCE_ROOT(SOURCE_ROOT)

ADD_SUBDIRECTORY(signals_test)

ADD_EXECUTABLE(distribution_data_test distribution_data_test.cpp)
ADD_EXECUTABLE(lamport_clock_test lamport_clock_test.cpp)
ADD_EXECUTABLE(dose_message_queue_test dose_message_queue_test.cpp)
ADD_EXECUTABLE(dose_sem_wrapper_test semaphore_test.cpp)


if (NOT MSVC)
  TARGET_LINK_LIBRARIES(distribution_data_test dose_internal dots_generated-Safir-cpp dots_cpp dots_id ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(lamport_clock_test dose_internal ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(dose_message_queue_test dose_internal dots_generated-Safir-cpp dots_generated-cpp dots_cpp lluf_utils lluf_crash_reporter ${Boost_LIBRARIES})
endif()

ENABLE_TESTING()
ADD_TEST(Semaphore dose_sem_wrapper_test)
ADD_TEST(MessageQueue dose_message_queue_test)
ADD_TEST(DistributionData distribution_data_test)
ADD_TEST(LamportClocks lamport_clock_test)

SET_TESTS_PROPERTIES(Semaphore PROPERTIES TIMEOUT 60)
SET_TESTS_PROPERTIES(MessageQueue PROPERTIES TIMEOUT 360)
SET_TESTS_PROPERTIES(LamportClocks PROPERTIES TIMEOUT 2400)

SET_TESTS_PROPERTIES(Semaphore PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")
SET_TESTS_PROPERTIES(MessageQueue PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")
SET_TESTS_PROPERTIES(DistributionData PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")
SET_TESTS_PROPERTIES(LamportClocks PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")
