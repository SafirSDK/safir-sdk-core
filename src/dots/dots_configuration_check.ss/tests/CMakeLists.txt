ADD_EXECUTABLE(dots_configuration_check_test dots_configuration_check_test.cpp ../src/dou_diff_helper.h)

TARGET_LINK_LIBRARIES(dots_configuration_check_test PRIVATE
  lluf_internal
  dots_internal
)

ADD_TEST(NAME dots_configuration_check_test COMMAND dots_configuration_check_test ${CMAKE_CURRENT_SOURCE_DIR}/dou)
SET_SAFIR_TEST_PROPERTIES(TEST dots_configuration_check_test)
