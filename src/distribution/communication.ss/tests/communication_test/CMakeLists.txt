ADD_EXECUTABLE(communication_test communication_test.cpp)

TARGET_LINK_LIBRARIES(communication_test PRIVATE
  communication
  lluf_id
  Boost::program_options
  Boost::thread
  Boost::system)

SAFIR_INSTALL(TARGETS communication_test COMPONENT TestSuite)
