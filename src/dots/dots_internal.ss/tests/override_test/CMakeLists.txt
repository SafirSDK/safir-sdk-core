FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
ADD_EXECUTABLE(dots_override_test ${sources} ${headers})

TARGET_LINK_LIBRARIES(dots_override_test PRIVATE
  dots_internal)

ADD_TEST(NAME dots_override_test_1 COMMAND dots_override_test ${CMAKE_CURRENT_SOURCE_DIR}/test/test1.txt)
SET_SAFIR_TEST_PROPERTIES(TEST dots_override_test_1)

ADD_TEST(NAME dots_override_test_2 COMMAND dots_override_test ${CMAKE_CURRENT_SOURCE_DIR}/test/test2.txt)
SET_SAFIR_TEST_PROPERTIES(TEST dots_override_test_2)

ADD_TEST(NAME dots_override_test_3 COMMAND dots_override_test ${CMAKE_CURRENT_SOURCE_DIR}/test/test3.txt)
SET_SAFIR_TEST_PROPERTIES(TEST dots_override_test_3)

ADD_TEST(NAME dots_override_test_4 COMMAND dots_override_test ${CMAKE_CURRENT_SOURCE_DIR}/test/test4.txt)
SET_SAFIR_TEST_PROPERTIES(TEST dots_override_test_4)

ADD_TEST(NAME dots_override_test_5 COMMAND dots_override_test ${CMAKE_CURRENT_SOURCE_DIR}/test/test5.txt)
SET_SAFIR_TEST_PROPERTIES(TEST dots_override_test_5)

ADD_TEST(NAME dots_override_test_6 COMMAND dots_override_test ${CMAKE_CURRENT_SOURCE_DIR}/test/test6.txt)
SET_SAFIR_TEST_PROPERTIES(TEST dots_override_test_6)

