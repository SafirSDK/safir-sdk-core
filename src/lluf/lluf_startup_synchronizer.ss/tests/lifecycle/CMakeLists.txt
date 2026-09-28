ADD_EXECUTABLE(ss_lifecycle_helper ss_lifecycle_helper.cpp)

TARGET_LINK_LIBRARIES(ss_lifecycle_helper PRIVATE
  lluf_startup_synchronizer
  Boost::filesystem
)

ADD_TEST(NAME StartupSynchronizer_lifecycle COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_lifecycle_tests.py
  --test-exe $<TARGET_FILE:ss_lifecycle_helper>)

SET_SAFIR_TEST_PROPERTIES(TEST StartupSynchronizer_lifecycle TIMEOUT 300)
