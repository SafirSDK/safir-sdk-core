add_library(external_time_provider_for_test SHARED test_provider.cpp)
safir_mark_single_abi(external_time_provider_for_test)

ADD_EXECUTABLE(ExternalTimeProvider_test test.cpp)

TARGET_LINK_LIBRARIES(ExternalTimeProvider_test PRIVATE
  douf_time_cpp)

ADD_TEST(NAME ExternalTimeProvider_test COMMAND ExternalTimeProvider_test)

SET_SAFIR_TEST_PROPERTIES(TEST ExternalTimeProvider_test
                          CONFIG_OVERRIDE "${CMAKE_CURRENT_SOURCE_DIR}/config")
