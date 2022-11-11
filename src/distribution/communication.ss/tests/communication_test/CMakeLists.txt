ADD_EXECUTABLE(communication_test communication_test.cpp)

TARGET_LINK_LIBRARIES(communication_test PRIVATE
  communication
  lluf_id
  lluf_internal
  Boost::program_options
  Boost::thread)

configure_file(communication_network.py
  "${CMAKE_CURRENT_BINARY_DIR}/communication_network.py")

SAFIR_INSTALL(TARGETS communication_test COMPONENT TestSuite)
