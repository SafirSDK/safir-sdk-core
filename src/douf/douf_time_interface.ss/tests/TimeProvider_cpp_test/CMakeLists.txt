ADD_EXECUTABLE(TimeProvider_cpp_test test.cpp)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(TimeProvider_cpp_test douf_time_cpp)
endif()

ADD_TEST(TimeProvider_cpp_test TimeProvider_cpp_test)
SET_SAFIR_TEST_PROPERTIES(TimeProvider_cpp_test)
