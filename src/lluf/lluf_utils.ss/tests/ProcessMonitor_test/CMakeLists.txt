ADD_EXECUTABLE(Sleeper Sleeper.cpp)

ADD_EXECUTABLE(ProcessMonitor_test3
  ProcessMonitor_test3.cpp
  ../../src/ProcessMonitor.cpp
  ../../src/ProcessMonitorLinux.cpp
  ../../src/ProcessMonitorWin32.cpp )

ADD_EXECUTABLE(ProcessMonitor_test ProcessMonitor_test.cpp)
ADD_EXECUTABLE(ProcessMonitor_test2 ProcessMonitor_test2.cpp)

if(NOT MSVC)
  TARGET_LINK_LIBRARIES (Sleeper ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES(ProcessMonitor_test3 ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES (ProcessMonitor_test lluf_utils ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES (ProcessMonitor_test2 lluf_utils ${Boost_LIBRARIES})
endif()

ADD_TEST(ProcessMonitor_unittest ProcessMonitor_test3)
SET_SAFIR_TEST_PROPERTIES(ProcessMonitor_unittest 120)

ADD_TEST(ProcessMonitor_monitor ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
ADD_TEST(ProcessMonitor_stop ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test2.py)

SET_SAFIR_TEST_PROPERTIES(ProcessMonitor_monitor)
SET_SAFIR_TEST_PROPERTIES(ProcessMonitor_stop)
