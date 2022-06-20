ADD_EXECUTABLE(consumer_cast_test consumer_cast_test.cpp)
ADD_EXECUTABLE(shared_memory_usage_test shared_memory_usage_test.cpp)

TARGET_LINK_LIBRARIES(consumer_cast_test PRIVATE
  dose_cpp)

TARGET_LINK_LIBRARIES(shared_memory_usage_test PRIVATE
  dose_cpp
  Boost::unit_test_framework)

ADD_TEST(NAME ConsumerCasting COMMAND consumer_cast_test)
ADD_TEST(NAME shared_memory_usage_test COMMAND shared_memory_usage_test)

SET_SAFIR_TEST_PROPERTIES(TEST ConsumerCasting)
SET_SAFIR_TEST_PROPERTIES(TEST shared_memory_usage_test)
