ADD_EXECUTABLE(safir_show_config show_config.cpp)

TARGET_LINK_LIBRARIES(safir_show_config PRIVATE 
  lluf_config 
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  ${Boost_FILESYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS safir_show_config)
