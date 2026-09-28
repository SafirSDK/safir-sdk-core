ADD_EXECUTABLE(ss_unit_test ss_unit_test.cpp)

TARGET_LINK_LIBRARIES(ss_unit_test PRIVATE
  lluf_startup_synchronizer
  lluf_config
  Boost::filesystem
  Boost::unit_test_framework
  )

ADD_TEST(NAME StartupSynchronizer_unit COMMAND ss_unit_test)
SET_SAFIR_TEST_PROPERTIES(TEST StartupSynchronizer_unit TIMEOUT 300)
