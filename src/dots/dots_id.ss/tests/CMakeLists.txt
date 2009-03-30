PROJECT (dots_id_test)
ENABLE_TESTING()
INCLUDE_DIRECTORIES($ENV{SAFIR_SDK}/include)
LINK_DIRECTORIES($ENV{SAFIR_RUNTIME}/lib)
ADD_EXECUTABLE(dots_id_test dots_id_test.cpp)

TARGET_LINK_LIBRARIES(dots_id_test dots_id)
ADD_TEST(dots_id dots_id_test)
