FILE(GLOB_RECURSE headers *.h)

ADD_LIBRARY(lluf_internal SHARED ${headers}
  LowLevelLoggerControl.cpp
  ProcessMonitor.cpp
  StringEncoding.cpp
  LowLevelLogger.cpp
  ProcessMonitorLinux.cpp
  SystemLog.cpp
  LogWin32.cpp
  ProcessMonitorWin32.cpp
)

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(lluf_internal PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

TARGET_LINK_LIBRARIES(lluf_internal PRIVATE
  lluf_utils

  PUBLIC
  $<BUILD_INTERFACE:lluf_config>
  $<BUILD_INTERFACE:Boost::filesystem>
  $<BUILD_INTERFACE:Boost::thread>)

SAFIR_INSTALL(
  TARGETS lluf_internal)
