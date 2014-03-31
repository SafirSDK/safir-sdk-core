FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(crash_dump_monitor ${sources})

FIND_PACKAGE(Boost COMPONENTS program_options filesystem thread system)

if(NOT MSVC)
  TARGET_LINK_LIBRARIES(crash_dump_monitor lluf_config lluf_utils ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS crash_dump_monitor 
  DESTINATION ${SAFIR_RUNTIME}/bin)
