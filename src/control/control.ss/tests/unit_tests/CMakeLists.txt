ADD_EXECUTABLE(SystemStateHandler_test SystemStateHandler_test.cpp ../../src/SystemStateHandler.h)

TARGET_LINK_LIBRARIES(SystemStateHandler_test PRIVATE
  system_picture
  ${Boost_SYSTEM_LIBRARY}
  ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})


ADD_TEST(NAME SystemStateHandler_test COMMAND SystemStateHandler_test)
SET_SAFIR_TEST_PROPERTIES(TEST SystemStateHandler_test TIMEOUT 600)

