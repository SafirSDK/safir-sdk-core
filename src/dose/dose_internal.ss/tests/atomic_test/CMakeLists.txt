
target_include_directories(signals_test PRIVATE ${safir-sdk-core_SOURCE_DIR}/src/include)

ADD_EXECUTABLE(Atomic_test atomic_test.cpp)

ADD_TEST(Atomic Atomic_test)
