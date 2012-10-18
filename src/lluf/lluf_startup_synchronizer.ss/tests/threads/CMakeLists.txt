ADD_EXECUTABLE(ss_threads_test ss_threads_test.cpp)

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES(ss_threads_test lluf_startup_synchronizer ${Boost_LIBRARIES})
ENDIF()

ADD_TEST(StartupSynchronizer_threads ss_threads_test)
set_tests_properties(StartupSynchronizer_threads PROPERTIES TIMEOUT 120)
