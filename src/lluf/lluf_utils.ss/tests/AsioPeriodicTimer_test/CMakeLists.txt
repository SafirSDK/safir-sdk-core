ADD_EXECUTABLE(AsioPeriodicTimer_test test.cpp)

TARGET_LINK_LIBRARIES(AsioPeriodicTimer_test PRIVATE ${Boost_SYSTEM_LIBRARY})

ADD_TEST(AsioPeriodicTimer AsioPeriodicTimer_test)
SET_SAFIR_TEST_PROPERTIES(TEST AsioPeriodicTimer TIMEOUT 40)
