ADD_EXECUTABLE(CheckCapabilities_test CheckCapabilities.cpp)
ADD_EXECUTABLE(BoostSleep_test BoostSleep_test.cpp)

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES (CheckCapabilities_test lluf_utils ${Boost_LIBRARIES})
  TARGET_LINK_LIBRARIES (BoostSleep_test lluf_utils ${Boost_LIBRARIES})
ENDIF()

ADD_TEST(CheckCapabilities CheckCapabilities_test)
ADD_TEST(BoostSleep BoostSleep_test)
SET_SAFIR_TEST_PROPERTIES(BoostSleep)
SET_SAFIR_TEST_PROPERTIES(CheckCapabilities)
