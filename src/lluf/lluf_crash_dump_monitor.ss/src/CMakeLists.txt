ADD_EXECUTABLE(crash_dump_monitor CrashDumpMonitor.cpp)

TARGET_LINK_LIBRARIES(crash_dump_monitor
  PRIVATE
  lluf_config
  lluf_utils
  Boost::filesystem
  Boost::program_options)

SAFIR_INSTALL(TARGETS crash_dump_monitor)
