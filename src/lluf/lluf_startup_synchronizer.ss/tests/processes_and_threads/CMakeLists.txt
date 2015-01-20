ADD_EXECUTABLE(ss_processes_and_threads_test ss_processes_and_threads_test.cpp)

TARGET_LINK_LIBRARIES(ss_processes_and_threads_test PRIVATE
  lluf_startup_synchronizer
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

ADD_TEST(NAME StartupSynchronizer_processes_and_threads COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
SET_SAFIR_TEST_PROPERTIES(TEST StartupSynchronizer_processes_and_threads TIMEOUT 120)
