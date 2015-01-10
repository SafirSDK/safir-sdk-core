FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_main_test_stub ${sources} ${headers})

TARGET_LINK_LIBRARIES(dose_main_test_stub
  system_picture
  control_utils
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

SAFIR_INSTALL(TARGETS dose_main_test_stub TEST_SUITE)
