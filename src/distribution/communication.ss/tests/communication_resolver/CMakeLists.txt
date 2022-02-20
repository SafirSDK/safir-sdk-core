ADD_EXECUTABLE(safir_resolver communication_resolver.cpp)

TARGET_LINK_LIBRARIES(safir_resolver PRIVATE
  Boost::program_options
  Boost::regex
  Boost::thread)

SAFIR_INSTALL(TARGETS safir_resolver COMPONENT Tools)
