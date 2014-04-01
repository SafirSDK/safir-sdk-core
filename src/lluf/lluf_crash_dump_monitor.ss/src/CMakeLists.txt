FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(crash_dump_monitor ${sources})

FIND_PACKAGE(Boost COMPONENTS program_options filesystem thread system)

TARGET_LINK_LIBRARIES(crash_dump_monitor lluf_config lluf_utils ${Boost_LIBRARIES})

INSTALL(TARGETS crash_dump_monitor 
  DESTINATION ${SAFIR_RUNTIME}/bin)
