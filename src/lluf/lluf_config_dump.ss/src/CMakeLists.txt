FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(safir_show_config ${sources})

TARGET_LINK_LIBRARIES(safir_show_config PRIVATE 
  lluf_config 
  ${Boost_PROGRAM_OPTIONS_LIBRARY})


INSTALL(TARGETS safir_show_config  
  DESTINATION ${SAFIR_RUNTIME}/bin)
