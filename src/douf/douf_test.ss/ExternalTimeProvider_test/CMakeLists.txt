add_library(external_time_provider_for_test SHARED test_provider.cpp)

ADD_EXECUTABLE(ExternalTimeProvider_test test.cpp)

TARGET_LINK_LIBRARIES(ExternalTimeProvider_test PRIVATE
  douf_time_cpp
  dots_cpp
  dots_internal
  lluf_utils)

ADD_TEST(ExternalTimeProvider_test ExternalTimeProvider_test)

SET_PROPERTY(TEST ExternalTimeProvider_test
    PROPERTY ENVIRONMENT 
    "SAFIR_TEST_CONFIG_OVERRIDE=${CMAKE_CURRENT_SOURCE_DIR}/config"
    "SAFIR_SOURCE_ROOT=${safir_sdk_core_SOURCE_DIR}/../"
    "CMAKE_CURRENT_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}")
