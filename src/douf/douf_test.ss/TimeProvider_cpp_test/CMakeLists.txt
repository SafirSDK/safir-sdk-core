ADD_EXECUTABLE(TimeProvider_cpp_test test.cpp)

TARGET_LINK_LIBRARIES(TimeProvider_cpp_test douf_time_cpp)

ADD_TEST(TimeProvider_cpp_test TimeProvider_cpp_test)

SET_PROPERTY(TEST TimeProvider_cpp_test
    PROPERTY ENVIRONMENT 
    "SAFIR_TEST_CONFIG_OVERRIDE=${CMAKE_CURRENT_SOURCE_DIR}/../config"
    "SAFIR_SOURCE_ROOT=${safir_sdk_core_SOURCE_DIR}/../")

