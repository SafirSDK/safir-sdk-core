#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

add_library(external_time_provider_for_test SHARED test_provider.cpp)

ADD_EXECUTABLE(ExternalTimeProvider_test test.cpp)

TARGET_LINK_LIBRARIES(ExternalTimeProvider_test PRIVATE
  douf_time_cpp
  dots_cpp
  dots_internal
  lluf_utils)

ADD_TEST(ExternalTimeProvider_test ExternalTimeProvider_test)

SET_SAFIR_TEST_PROPERTIES(TEST ExternalTimeProvider_test
                          CONFIG_OVERRIDE "${CMAKE_CURRENT_SOURCE_DIR}/config")
