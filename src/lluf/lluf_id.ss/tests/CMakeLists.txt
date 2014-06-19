ADD_EXECUTABLE(lluf_id_test lluf_id_test.cpp)

TARGET_LINK_LIBRARIES(lluf_id_test PRIVATE lluf_id)

ADD_TEST(NAME lluf_id COMMAND lluf_id_test)

SET_SAFIR_TEST_PROPERTIES(TEST lluf_id)
