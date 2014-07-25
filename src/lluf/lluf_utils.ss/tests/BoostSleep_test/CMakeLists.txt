ADD_EXECUTABLE(CheckCapabilities_test CheckCapabilities.cpp)

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES (CheckCapabilities_test lluf_utils ${Boost_LIBRARIES})
ENDIF()

ADD_TEST(CheckCapabilities CheckCapabilities_test)
SET_SAFIR_TEST_PROPERTIES(CheckCapabilities)
