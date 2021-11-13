ADD_EXECUTABLE(system_picture_component_test_node system_picture_component_test_node.cpp)

TARGET_LINK_LIBRARIES(system_picture_component_test_node PRIVATE
  system_picture
  lluf_crash_reporter
  lluf_utils
  lluf_id
  Boost::program_options)

SAFIR_INSTALL(TARGETS system_picture_component_test_node COMPONENT TestSuite)

SAFIR_INSTALL_PYTHON(SCRIPT circular_restart.py
  COMPONENT TestSuite)

INSTALL(
  DIRECTORY
    config
  DESTINATION ${SAFIR_INSTALL_DESTINATION_TEST_DATA}/system_picture
  COMPONENT TestSuite
  )
