ADD_EXECUTABLE(CheckCapabilities_test CheckCapabilities.cpp)
ADD_EXECUTABLE(BoostSleep_test BoostSleep_test.cpp)

TARGET_LINK_LIBRARIES (CheckCapabilities_test PRIVATE lluf_utils
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

TARGET_LINK_LIBRARIES (BoostSleep_test PRIVATE 
  lluf_utils
  ${Boost_TIMER_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

ADD_TEST(CheckCapabilities CheckCapabilities_test)
ADD_TEST(BoostSleep BoostSleep_test)
SET_SAFIR_TEST_PROPERTIES(TEST BoostSleep TIMEOUT 600)
SET_SAFIR_TEST_PROPERTIES(TEST CheckCapabilities)
