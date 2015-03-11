ADD_EXECUTABLE(communication_resolver communication_resolver.cpp)

TARGET_LINK_LIBRARIES(communication_resolver PRIVATE
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS communication_resolver TEST_SUITE)
