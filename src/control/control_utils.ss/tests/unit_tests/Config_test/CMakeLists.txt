ADD_EXECUTABLE(Config_test Config_test.cpp)

TARGET_LINK_LIBRARIES(Config_test PRIVATE
  lluf_id
  lluf_config
  dots_internal
  safir_generated-Core-cpp
  Boost::filesystem)

ADD_TEST(NAME Config_test COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py ${CMAKE_CURRENT_SOURCE_DIR}/testconf --test-exe $<TARGET_FILE:Config_test>)
SET_SAFIR_TEST_PROPERTIES(TEST Config_test)
