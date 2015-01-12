ADD_EXECUTABLE(control_stub sp_control.cpp)
ADD_EXECUTABLE(dose_main_stub dose_main_stub.cpp)

TARGET_LINK_LIBRARIES(control_stub
  system_picture
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  )

TARGET_LINK_LIBRARIES(dose_main_stub
  system_picture
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  )

SAFIR_INSTALL(TARGETS control_stub dose_main_stub TEST_SUITE)

SAFIR_INSTALL_PYTHON(SCRIPT start_system_picture_test.py
  COMPONENT Test)

SAFIR_INSTALL_PYTHON(SCRIPT circular_restart.py
  COMPONENT Test)
