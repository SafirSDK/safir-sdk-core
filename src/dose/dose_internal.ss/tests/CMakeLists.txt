FIND_PACKAGE(Boost COMPONENTS system thread)

ADD_SUBDIRECTORY(signals_test)

ADD_EXECUTABLE(distribution_data_test distribution_data_test.cpp)
ADD_EXECUTABLE(lamport_clock_test lamport_clock_test.cpp)
ADD_EXECUTABLE(dose_message_queue_test dose_message_queue_test.cpp)
ADD_EXECUTABLE(dose_sem_wrapper_test semaphore_test.cpp)

TARGET_LINK_LIBRARIES(distribution_data_test dose_internal dots_generated-Safir-cpp dots_cpp lluf_id ${Boost_LIBRARIES})
TARGET_LINK_LIBRARIES(lamport_clock_test dose_internal ${Boost_LIBRARIES})
TARGET_LINK_LIBRARIES(dose_message_queue_test dose_internal dots_generated-Safir-cpp dots_cpp lluf_utils lluf_crash_reporter ${Boost_LIBRARIES})
target_include_directories(dose_sem_wrapper_test PRIVATE ../src)

ADD_TEST(Semaphore dose_sem_wrapper_test)
ADD_TEST(MessageQueue dose_message_queue_test)
ADD_TEST(DistributionData distribution_data_test)
ADD_TEST(LamportClocks lamport_clock_test)

SET_SAFIR_TEST_PROPERTIES(TEST Semaphore TIMEOUT 60)
SET_SAFIR_TEST_PROPERTIES(TEST MessageQueue TIMEOUT 360)
SET_SAFIR_TEST_PROPERTIES(TEST DistributionData)
SET_SAFIR_TEST_PROPERTIES(TEST LamportClocks TIMEOUT 2400)


#TODO: generally: Check that only "external" libraries use autolink. Applies to all libraries, this is a general reminder
