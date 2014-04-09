ADD_EXECUTABLE(ProcessMonitor_test ProcessMonitor_test.cpp)
ADD_EXECUTABLE(ProcessMonitor_test2 ProcessMonitor_test2.cpp)
ADD_EXECUTABLE(Sleeper Sleeper.cpp)

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES (Sleeper ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES (ProcessMonitor_test lluf_utils ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES (ProcessMonitor_test2 lluf_utils ${Boost_LIBRARIES})
ENDIF()

ADD_TEST(ProcessMonitor_monitor ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
ADD_TEST(ProcessMonitor_stop ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test2.py)

SET_SAFIR_TEST_PROPERTIES(TEST ProcessMonitor_monitor)
SET_SAFIR_TEST_PROPERTIES(TEST ProcessMonitor_stop)
