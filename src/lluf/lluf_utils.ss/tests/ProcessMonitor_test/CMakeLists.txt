ADD_EXECUTABLE(ProcessMonitor_test3
  ProcessMonitor_test3.cpp
  ../../src/ProcessMonitor.cpp
  ../../src/ProcessMonitorLinux.cpp
  ../../src/ProcessMonitorWin32.cpp)

TARGET_INCLUDE_DIRECTORIES(ProcessMonitor_test3 PRIVATE ../../src/include)

ADD_EXECUTABLE(ProcessMonitor_test ProcessMonitor_test.cpp)
ADD_EXECUTABLE(ProcessMonitor_test2 ProcessMonitor_test2.cpp)
ADD_EXECUTABLE(ProcessMonitorSleeper Sleeper.cpp)

if(MSVC)
    set_target_properties(ProcessMonitor_test3 PROPERTIES
    COMPILE_DEFINITIONS " -D_lluf_utils_export_defs_h -Dlluf_utils_EXPORTS")
endif()


TARGET_LINK_LIBRARIES (ProcessMonitor_test3 PRIVATE
  lluf_utils
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

TARGET_LINK_LIBRARIES (ProcessMonitor_test PRIVATE
  lluf_utils
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

TARGET_LINK_LIBRARIES (ProcessMonitor_test2 PRIVATE
  lluf_utils
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

TARGET_LINK_LIBRARIES (ProcessMonitorSleeper PRIVATE
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

ADD_TEST(NAME ProcessMonitor_unittest COMMAND ProcessMonitor_test3)
ADD_TEST(NAME ProcessMonitor_monitor COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
ADD_TEST(NAME ProcessMonitor_stop COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test2.py)

SET_SAFIR_TEST_PROPERTIES(TEST ProcessMonitor_unittest)
SET_SAFIR_TEST_PROPERTIES(TEST ProcessMonitor_monitor)
SET_SAFIR_TEST_PROPERTIES(TEST ProcessMonitor_stop)
