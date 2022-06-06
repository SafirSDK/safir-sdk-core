INCLUDE_DIRECTORIES(../../src/include)
ADD_EXECUTABLE(Atomic_test atomic_test.cpp)

ADD_TEST(NAME Atomic COMMAND Atomic_test)
