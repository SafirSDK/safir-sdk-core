set(headers ControlApp.h SystemStateHandler.h IncarnationBlackListHandler.h StopHandler.h CommandExecutor.h TerminateHandler.h)
set(sources safir_control.cpp ControlApp.cpp TerminateHandler.cpp)

ADD_EXECUTABLE(safir_control ${sources} ${headers})

TARGET_LINK_LIBRARIES(safir_control PRIVATE
  system_picture
  control_utils
  lluf_internal
  lluf_crash_reporter
  Boost::filesystem
  Boost::program_options)

SAFIR_INSTALL(TARGETS safir_control)
