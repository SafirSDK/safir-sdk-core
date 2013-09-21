FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(ProcessInfo_test ${sources})

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES (ProcessInfo_test lluf_utils ${Boost_LIBRARIES})
ENDIF()

ADD_TEST(ProcessInfo ProcessInfo_test)
SET_SAFIR_TEST_PROPERTIES(ProcessInfo)
