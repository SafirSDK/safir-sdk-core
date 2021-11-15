ADD_EXECUTABLE(AsioPeriodicTimer_test test.cpp)

TARGET_LINK_LIBRARIES(AsioPeriodicTimer_test PRIVATE
    Boost::atomic
    Boost::system)

ADD_TEST(NAME AsioPeriodicTimer COMMAND AsioPeriodicTimer_test)
SET_SAFIR_TEST_PROPERTIES(TEST AsioPeriodicTimer)
