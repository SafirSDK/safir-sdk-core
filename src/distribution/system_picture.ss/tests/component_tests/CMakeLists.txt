ADD_EXECUTABLE(sp_test_ctrl sp_test_ctrl.cpp)
ADD_EXECUTABLE(sp_test_dm sp_test_dm.cpp)

TARGET_LINK_LIBRARIES(sp_test_ctrl PRIVATE
  system_picture
  lluf_crash_reporter
  lluf_utils
  lluf_id
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

TARGET_LINK_LIBRARIES(sp_test_dm PRIVATE
  system_picture
  lluf_crash_reporter
  lluf_utils
  lluf_id
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

SAFIR_INSTALL(TARGETS sp_test_ctrl sp_test_dm TEST_SUITE)

SAFIR_INSTALL_PYTHON(SCRIPT start_system_picture_test.py
  COMPONENT Test)

SAFIR_INSTALL_PYTHON(SCRIPT circular_restart.py
  COMPONENT Test)
