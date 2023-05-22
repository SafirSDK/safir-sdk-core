ADD_EXECUTABLE(ProcessMonitor_test3
  ProcessMonitor_test3.cpp
  ../../src/ProcessMonitor.cpp
  ../../src/ProcessMonitorLinux.cpp
  ../../src/ProcessMonitorWin32.cpp)

TARGET_INCLUDE_DIRECTORIES(ProcessMonitor_test3 PRIVATE ../../src/include)

ADD_EXECUTABLE(ProcessMonitor_polling_test
  ProcessMonitor_test3.cpp
  ../../src/ProcessMonitor.cpp
  ../../src/ProcessMonitorLinux.cpp
  ../../src/ProcessMonitorWin32.cpp)

TARGET_INCLUDE_DIRECTORIES(ProcessMonitor_polling_test PRIVATE ../../src/include)


ADD_EXECUTABLE(ProcessMonitor_test ProcessMonitor_test.cpp)
ADD_EXECUTABLE(ProcessMonitor_test2 ProcessMonitor_test2.cpp)
ADD_EXECUTABLE(ProcessMonitorSleeper Sleeper.cpp)

if(MSVC)
    set_target_properties(ProcessMonitor_test3 PROPERTIES
    COMPILE_DEFINITIONS "_lluf_internal_export_defs_h;lluf_internal_EXPORTS")

    set_target_properties(ProcessMonitor_polling_test PROPERTIES
    COMPILE_DEFINITIONS "PROCMONTEST_NO_OPENPROCESS;_lluf_internal_export_defs_h;lluf_internal_EXPORTS")
endif()


TARGET_LINK_LIBRARIES (ProcessMonitor_test3 PRIVATE
  lluf_internal
  Boost::unit_test_framework
  Boost::filesystem)

  TARGET_LINK_LIBRARIES (ProcessMonitor_polling_test PRIVATE
  lluf_internal
  Boost::unit_test_framework
  Boost::filesystem)

TARGET_LINK_LIBRARIES (ProcessMonitor_test PRIVATE
  lluf_internal)

TARGET_LINK_LIBRARIES (ProcessMonitor_test2 PRIVATE
  lluf_internal)

TARGET_LINK_LIBRARIES (ProcessMonitorSleeper PRIVATE
  Boost::thread)

ADD_TEST(NAME ProcessMonitor_unittest
  COMMAND ProcessMonitor_test3
            --catch_system_error=no
            -- --sleeper-exe $<TARGET_FILE:ProcessMonitorSleeper>)

ADD_TEST(NAME ProcessMonitor_polling_unittest
  COMMAND ProcessMonitor_polling_test
           --catch_system_error=no
           -- --sleeper-exe $<TARGET_FILE:ProcessMonitorSleeper>)

ADD_TEST(NAME ProcessMonitor_monitor COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
                   --test-exe $<TARGET_FILE:ProcessMonitor_test>
                   --sleeper-exe $<TARGET_FILE:ProcessMonitorSleeper>)

ADD_TEST(NAME ProcessMonitor_stop COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test2.py
                   --test-exe $<TARGET_FILE:ProcessMonitor_test2>
                   --sleeper-exe $<TARGET_FILE:ProcessMonitorSleeper>)

SET_SAFIR_TEST_PROPERTIES(TEST ProcessMonitor_unittest TIMEOUT 240)
SET_SAFIR_TEST_PROPERTIES(TEST ProcessMonitor_polling_unittest TIMEOUT 240)
SET_SAFIR_TEST_PROPERTIES(TEST ProcessMonitor_monitor)
SET_SAFIR_TEST_PROPERTIES(TEST ProcessMonitor_stop)
