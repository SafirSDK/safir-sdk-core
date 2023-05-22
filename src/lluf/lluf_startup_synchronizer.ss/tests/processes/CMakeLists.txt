FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(ss_test ${sources})

TARGET_LINK_LIBRARIES(ss_test PRIVATE
  lluf_startup_synchronizer
  Boost::thread
)

ADD_TEST(NAME StartupSynchronizer_processes COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --test-exe $<TARGET_FILE:ss_test>)
SET_SAFIR_TEST_PROPERTIES(TEST StartupSynchronizer_processes)
