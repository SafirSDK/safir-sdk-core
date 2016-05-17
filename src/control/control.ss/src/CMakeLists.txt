#first we try to see if boost.process is already installed on the computer
#if it is not we will have to download our own copy of it.
FIND_PATH(BOOST_PROCESS_INCLUDE_DIRECTORY
  NAMES
    boost/process.hpp
  PATHS
    /usr/include
    /usr/local/include)



set(headers ControlApp.h SystemStateHandler.h IncarnationBlackListHandler.h StopHandler.h CommandExecutor.h TerminateHandler.h)
set(sources safir_control.cpp ControlApp.cpp TerminateHandler.cpp)

ADD_EXECUTABLE(safir_control ${sources} ${headers})

TARGET_LINK_LIBRARIES(safir_control PRIVATE
  system_picture
  control_utils
  lluf_utils
  lluf_crash_reporter
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

target_include_directories(safir_control PRIVATE boost-process)

SAFIR_INSTALL(TARGETS safir_control)
