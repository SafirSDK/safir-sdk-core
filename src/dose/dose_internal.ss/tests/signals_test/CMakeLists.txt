ADD_EXECUTABLE(signals_test test.cpp ../../src/Signals.cpp)

target_include_directories(signals_test PRIVATE ../../src)

#disable lock checks
add_definitions(-DDOSE_NO_LOCK_CHECKING)

TARGET_LINK_LIBRARIES(signals_test dots_generated-Safir-cpp dots_cpp lluf_utils ${Boost_LIBRARIES})

ADD_TEST(Signals signals_test)

SET_SAFIR_TEST_PROPERTIES(Signals 180)
