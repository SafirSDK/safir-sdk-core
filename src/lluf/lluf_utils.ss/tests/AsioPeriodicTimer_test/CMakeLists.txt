ADD_EXECUTABLE(AsioPeriodicTimer_test test.cpp)

IF (NOT MSVC)
   TARGET_LINK_LIBRARIES (AsioPeriodicTimer_test ${Boost_LIBRARIES})
ENDIF()

ADD_TEST(AsioPeriodicTimer AsioPeriodicTimer_test)
set_tests_properties(AsioPeriodicTimer PROPERTIES TIMEOUT 40)
