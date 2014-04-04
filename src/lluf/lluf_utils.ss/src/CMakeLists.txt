

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

target_include_directories(lluf_utils PUBLIC .)

#TODO: remove date_time dependency (we want date_time to be header only, but theres
#something that makes that not work on windows)

FIND_PACKAGE(Boost COMPONENTS filesystem system thread program_options date_time)

TARGET_LINK_LIBRARIES(lluf_utils lluf_startup_synchronizer lluf_config ${Boost_LIBRARIES})

if (WIN32)
  TARGET_LINK_LIBRARIES(lluf_utils psapi)
else()
  TARGET_LINK_LIBRARIES(lluf_utils dl rt)
endif()


ADD_EXECUTABLE(logger_control logger_control.cpp)

FIND_PACKAGE(Boost COMPONENTS filesystem program_options system)

TARGET_LINK_LIBRARIES(logger_control lluf_utils ${Boost_LIBRARIES})




INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS lluf_utils 
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)


INSTALL_DEBUG_INFO(lluf_utils)

if (WIN32)
INSTALL(FILES ../data/safir_event_log_registration.reg
        DESTINATION ${SAFIR_RUNTIME}/data/text/)
endif()


INSTALL(TARGETS logger_control 
  DESTINATION ${SAFIR_RUNTIME}/bin)
