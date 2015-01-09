ADD_EXECUTABLE(Atomic_test atomic_test.cpp)

#target_include_directories(Atomic_test PRIVATE ${safir-sdk-core_SOURCE_DIR}/src/include)

ADD_TEST(Atomic Atomic_test)
