ADD_EXECUTABLE(ss_threads_test ss_threads_test.cpp)

TARGET_LINK_LIBRARIES(ss_threads_test PRIVATE 
  lluf_startup_synchronizer
  ${Boost_THREAD_LIBRARY} 
  ${Boost_SYSTEM_LIBRARY})

ADD_TEST(StartupSynchronizer_threads ss_threads_test)
SET_SAFIR_TEST_PROPERTIES(TEST StartupSynchronizer_threads TIMEOUT 120)
