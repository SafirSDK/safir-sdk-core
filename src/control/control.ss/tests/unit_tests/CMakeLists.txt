ADD_EXECUTABLE(SystemStateHandler_test SystemStateHandler_test.cpp ../../src/SystemStateHandler.h)
ADD_EXECUTABLE(StopHandler_test StopHandler_test.cpp ../../src/StopHandler.h)
ADD_EXECUTABLE(IncarnationBlackListHandler_test IncarnationBlackListHandler_test.cpp ../../src/IncarnationBlackListHandler.h)

TARGET_LINK_LIBRARIES(SystemStateHandler_test PRIVATE
  system_picture
  lluf_internal
  Boost::unit_test_framework)

TARGET_LINK_LIBRARIES(StopHandler_test PRIVATE
  system_picture
  control_utils
  lluf_internal
  Boost::unit_test_framework)

TARGET_LINK_LIBRARIES(IncarnationBlackListHandler_test PRIVATE
    lluf_internal
    Boost::filesystem
    Boost::unit_test_framework)

ADD_TEST(NAME SystemStateHandler_test COMMAND SystemStateHandler_test)
SET_SAFIR_TEST_PROPERTIES(TEST SystemStateHandler_test TIMEOUT 600)

ADD_TEST(NAME StopHandler_test COMMAND StopHandler_test)
SET_SAFIR_TEST_PROPERTIES(TEST StopHandler_test TIMEOUT 600)

ADD_TEST(NAME IncarnationBlackListHandler_test COMMAND IncarnationBlackListHandler_test)
SET_SAFIR_TEST_PROPERTIES(TEST IncarnationBlackListHandler_test TIMEOUT 600)
