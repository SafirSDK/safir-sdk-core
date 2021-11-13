ADD_EXECUTABLE(ss_threads_test ss_threads_test.cpp)

TARGET_LINK_LIBRARIES(ss_threads_test PRIVATE
  lluf_startup_synchronizer
  Boost::thread
  Boost::system)

ADD_TEST(NAME StartupSynchronizer_threads COMMAND ss_threads_test)
SET_SAFIR_TEST_PROPERTIES(TEST StartupSynchronizer_threads)
