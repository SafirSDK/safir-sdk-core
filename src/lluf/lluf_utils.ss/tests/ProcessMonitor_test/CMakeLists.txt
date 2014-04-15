ADD_EXECUTABLE(ProcessMonitor_test ProcessMonitor_test.cpp)
ADD_EXECUTABLE(ProcessMonitor_test2 ProcessMonitor_test2.cpp)
ADD_EXECUTABLE(ProcessMonitorSleeper Sleeper.cpp)

TARGET_LINK_LIBRARIES (ProcessMonitorSleeper PRIVATE ${Boost_THREAD_LIBRARY} ${Boost_SYSTEM_LIBRARY})

TARGET_LINK_LIBRARIES (ProcessMonitor_test PRIVATE 
  lluf_utils
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

TARGET_LINK_LIBRARIES (ProcessMonitor_test2 PRIVATE
  lluf_utils
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

ADD_TEST(ProcessMonitor_monitor ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
ADD_TEST(ProcessMonitor_stop ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test2.py)

SET_SAFIR_TEST_PROPERTIES(TEST ProcessMonitor_monitor)
SET_SAFIR_TEST_PROPERTIES(TEST ProcessMonitor_stop)
