ADD_EXECUTABLE(signals_test test.cpp ../../src/Signals.cpp)

target_include_directories(signals_test PRIVATE ../../src/include)

#disable lock checks
add_definitions(-DDOSE_NO_LOCK_CHECKING)

TARGET_LINK_LIBRARIES(signals_test PRIVATE
  lluf_utils
  safir_generated-Core-cpp)

ADD_TEST(NAME Signals COMMAND signals_test)

SET_SAFIR_TEST_PROPERTIES(TEST Signals TIMEOUT 180)
