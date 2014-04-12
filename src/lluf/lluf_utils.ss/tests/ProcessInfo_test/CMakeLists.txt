FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(ProcessInfo_test ${sources})

TARGET_LINK_LIBRARIES (ProcessInfo_test lluf_utils)

ADD_TEST(ProcessInfo ProcessInfo_test)
SET_SAFIR_TEST_PROPERTIES(TEST ProcessInfo)
