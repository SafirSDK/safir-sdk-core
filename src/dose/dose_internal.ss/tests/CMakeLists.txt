ADD_SUBDIRECTORY(signals_test)
ADD_SUBDIRECTORY(lamport_clock_test)

ADD_EXECUTABLE(distribution_data_test distribution_data_test.cpp)
ADD_EXECUTABLE(dose_message_queue_test dose_message_queue_test.cpp)
ADD_EXECUTABLE(dose_sem_wrapper_test semaphore_test.cpp)
ADD_EXECUTABLE(wrap_around_counter_test wrap_around_counter_test.cpp)

TARGET_LINK_LIBRARIES(distribution_data_test PRIVATE
  lluf_internal
  dose_internal)

TARGET_LINK_LIBRARIES(dose_message_queue_test PRIVATE
  lluf_internal
  dose_internal
  lluf_crash_reporter)

TARGET_LINK_LIBRARIES(dose_sem_wrapper_test PRIVATE
  lluf_config)

target_include_directories(dose_sem_wrapper_test PRIVATE ../src/include)
target_include_directories(wrap_around_counter_test PRIVATE ../src/include)

ADD_TEST(NAME Semaphore COMMAND dose_sem_wrapper_test)
ADD_TEST(NAME MessageQueue COMMAND dose_message_queue_test)
ADD_TEST(NAME DistributionData COMMAND distribution_data_test)
ADD_TEST(NAME WrapAroundCounter COMMAND wrap_around_counter_test)

SET_SAFIR_TEST_PROPERTIES(TEST Semaphore)
SET_SAFIR_TEST_PROPERTIES(TEST MessageQueue TIMEOUT 360)
SET_SAFIR_TEST_PROPERTIES(TEST DistributionData)
SET_SAFIR_TEST_PROPERTIES(TEST WrapAroundCounter TIMEOUT 600)
