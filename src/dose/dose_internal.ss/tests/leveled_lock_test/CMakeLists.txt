ADD_EXECUTABLE(leveled_lock_test leveled_lock_test.cpp)

TARGET_LINK_LIBRARIES(leveled_lock_test PRIVATE
  dose_internal
  Boost::unit_test_framework)

ADD_TEST(NAME LeveledLock COMMAND leveled_lock_test)

SET_SAFIR_TEST_PROPERTIES(TEST LeveledLock TIMEOUT 120)
