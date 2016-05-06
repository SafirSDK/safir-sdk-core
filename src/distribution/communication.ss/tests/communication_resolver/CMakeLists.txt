ADD_EXECUTABLE(safir_resolver communication_resolver.cpp)

TARGET_LINK_LIBRARIES(safir_resolver PRIVATE
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  ${Boost_REGEX_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS safir_resolver COMPONENT Tools)
