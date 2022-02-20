FILE(GLOB_RECURSE headers *.h)

ADD_LIBRARY(lluf_utils SHARED ${headers}
  DynamicLibraryLoader.cpp
  LowLevelLoggerControl.cpp
  ProcessMonitor.cpp
  StringEncoding.cpp
  LowLevelLogger.cpp
  ProcessMonitorLinux.cpp
  SystemLog.cpp
  LogWin32.cpp
  ProcessInfo.cpp
  ProcessMonitorWin32.cpp
)

#Make our INTERNAL headers available to other parts of Core.
target_include_directories(lluf_utils PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

#Some libraries are used in our internal headers, not in the public headers so we
#only make them public for the Core tree build, not for the exported cmake
#configuration. This allows for other parts of core to include e.g. LowLevelLogger
#without having to add thread and system to their link dependencies.

TARGET_LINK_LIBRARIES(lluf_utils PRIVATE
  PUBLIC
  $<BUILD_INTERFACE:lluf_config>
  $<BUILD_INTERFACE:Boost::filesystem>
  $<BUILD_INTERFACE:Boost::thread>)

if (WIN32)
  TARGET_LINK_LIBRARIES(lluf_utils PRIVATE lluf_startup_synchronizer psapi)
else()
  TARGET_LINK_LIBRARIES(lluf_utils PRIVATE dl rt)
endif()

SAFIR_INSTALL(
  TARGETS lluf_utils)
