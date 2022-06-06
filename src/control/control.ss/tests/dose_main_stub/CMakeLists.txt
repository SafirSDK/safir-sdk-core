FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_main_test_stub ${sources} ${headers})

TARGET_LINK_LIBRARIES(dose_main_test_stub PRIVATE
  control_utils
  system_picture
  lluf_internal
  Boost::program_options)

SAFIR_INSTALL(TARGETS dose_main_test_stub COMPONENT TestSuite)
