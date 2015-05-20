ADD_EXECUTABLE(communication_test communication_test.cpp)

TARGET_LINK_LIBRARIES(communication_test PRIVATE
  communication
  lluf_id
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS communication_test TEST_SUITE)
