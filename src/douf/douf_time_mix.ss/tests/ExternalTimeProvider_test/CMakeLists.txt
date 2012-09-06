add_library(external_time_provider_for_test SHARED test_provider.cpp)

ADD_EXECUTABLE(ExternalTimeProvider_test test.cpp)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(ExternalTimeProvider_test douf_time_cpp)
endif()

ADD_TEST(ExternalTimeProvider_test ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
set_tests_properties(ExternalTimeProvider_test PROPERTIES TIMEOUT 60)

