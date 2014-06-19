ADD_EXECUTABLE(TimeProvider_cpp_test test.cpp)

TARGET_LINK_LIBRARIES(TimeProvider_cpp_test PRIVATE
  douf_time_cpp)

#Needed on windows
if (MSVC)
  TARGET_LINK_LIBRARIES(TimeProvider_cpp_test PRIVATE ${Boost_DATE_TIME_LIBRARY})
endif()


ADD_TEST(NAME TimeProvider_cpp_test COMMAND TimeProvider_cpp_test)

SET_SAFIR_TEST_PROPERTIES(TEST TimeProvider_cpp_test)
