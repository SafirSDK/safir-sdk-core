ADD_EXECUTABLE(consumer_cast_test consumer_cast_test.cpp)

TARGET_LINK_LIBRARIES(consumer_cast_test PRIVATE
  dose_cpp)

ADD_TEST(NAME ConsumerCasting COMMAND consumer_cast_test)

SET_SAFIR_TEST_PROPERTIES(TEST ConsumerCasting TIMEOUT 60)
