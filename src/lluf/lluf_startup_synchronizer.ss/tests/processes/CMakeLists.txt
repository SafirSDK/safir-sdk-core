FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(ss_test ${sources})

TARGET_LINK_LIBRARIES(ss_test PRIVATE 
  lluf_startup_synchronizer 
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

ADD_TEST(NAME StartupSynchronizer_processes COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
SET_SAFIR_TEST_PROPERTIES(TEST StartupSynchronizer_processes TIMEOUT 120)
