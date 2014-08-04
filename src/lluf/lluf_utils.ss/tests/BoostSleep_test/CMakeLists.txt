ADD_EXECUTABLE(CheckCapabilities_test CheckCapabilities.cpp)

TARGET_LINK_LIBRARIES (CheckCapabilities_test PRIVATE lluf_utils
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

ADD_TEST(CheckCapabilities CheckCapabilities_test)
SET_SAFIR_TEST_PROPERTIES(TEST CheckCapabilities)
