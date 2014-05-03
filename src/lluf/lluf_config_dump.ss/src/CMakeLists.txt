FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(safir_show_config ${sources})

TARGET_LINK_LIBRARIES(safir_show_config PRIVATE 
  lluf_config 
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

SAFIR_INSTALL(TARGETS safir_show_config)
