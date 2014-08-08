
target_include_directories(signals_test PRIVATE ${safir_sdk_core_SOURCE_DIR}/src/include)

ADD_EXECUTABLE(Atomic_test atomic_test.cpp)

ADD_TEST(Atomic Atomic_test)
