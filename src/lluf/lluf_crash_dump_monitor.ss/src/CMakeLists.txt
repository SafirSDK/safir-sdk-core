FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(crash_dump_monitor ${sources})

TARGET_LINK_LIBRARIES(crash_dump_monitor 
  PRIVATE
  lluf_config
  lluf_utils 
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS crash_dump_monitor)
