FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(ss_test ${sources})

TARGET_LINK_LIBRARIES(ss_test lluf_startup_synchronizer ${Boost_LIBRARIES})

ADD_TEST(StartupSynchronizer_processes ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
SET_SAFIR_TEST_PROPERTIES(TEST StartupSynchronizer_processes TIMEOUT 120)
