set(sources control_cli.cpp)

ADD_EXECUTABLE(safir_control_cli ${sources} ${headers})

TARGET_LINK_LIBRARIES(safir_control_cli PRIVATE
  control_utils
  logging_cpp
  Boost::program_options)

SAFIR_INSTALL(TARGETS safir_control_cli)
