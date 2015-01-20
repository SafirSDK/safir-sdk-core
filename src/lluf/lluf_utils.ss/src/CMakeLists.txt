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

TARGET_LINK_LIBRARIES(lluf_utils PRIVATE
  lluf_startup_synchronizer)

#Some libraries are used in our internal headers, not in the public headers so we
#only make them public for the Core tree build, not for the exported cmake
#configuration. This allows for other parts of core to include e.g. LowLevelLogger
#without having to add thread and system to their link dependencies.
#The reason for having different code on windows and Linux is a bug in cmake, which
#makes the correct code (which is in the Linux branch) not work on Windows.
if (MSVC)
  TARGET_LINK_LIBRARIES(lluf_utils PRIVATE
    PUBLIC
    $<BUILD_INTERFACE:lluf_config>
    ${Boost_FILESYSTEM_LIBRARY}
    ${Boost_THREAD_LIBRARY}
	${Boost_SYSTEM_LIBRARY})
else()
  TARGET_LINK_LIBRARIES(lluf_utils PRIVATE
    PUBLIC
    $<BUILD_INTERFACE:lluf_config>
    "$<BUILD_INTERFACE:${Boost_FILESYSTEM_LIBRARY}>"
    "$<BUILD_INTERFACE:${Boost_THREAD_LIBRARY}>"
	"$<BUILD_INTERFACE:${Boost_SYSTEM_LIBRARY}>")
endif()

if (WIN32)
  TARGET_LINK_LIBRARIES(lluf_utils PRIVATE psapi)
else()
  TARGET_LINK_LIBRARIES(lluf_utils PRIVATE dl rt)
endif()

ADD_EXECUTABLE(logger_control logger_control.cpp)

TARGET_LINK_LIBRARIES(logger_control PRIVATE
  lluf_utils
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

SAFIR_INSTALL(
  TARGETS lluf_utils logger_control)

if (WIN32)
  INSTALL(FILES ../data/safir_event_log_registration.reg
    DESTINATION ${SAFIR_INSTALL_DESTINATION_DOC}/utilities/)
endif()
