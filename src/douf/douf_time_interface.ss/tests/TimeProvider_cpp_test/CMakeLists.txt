ADD_EXECUTABLE(TimeProvider_cpp_test test.cpp)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(TimeProvider_cpp_test douf_time_cpp)
endif()

ADD_TEST(TimeProvider_cpp_test TimeProvider_cpp_test)
SET_TESTS_PROPERTIES(TimeProvider_cpp_test PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")
