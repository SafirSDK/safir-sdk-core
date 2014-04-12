ADD_EXECUTABLE(consumer_cast_test consumer_cast_test.cpp)

TARGET_LINK_LIBRARIES(consumer_cast_test PRIVATE 
  dose_cpp
  dose_internal 
  dots_generated-Safir-cpp
  dots_cpp
  dots_kernel
  dots_internal
  lluf_utils)


ADD_TEST(ConsumerCasting consumer_cast_test)

SET_SAFIR_TEST_PROPERTIES(TEST ConsumerCasting TIMEOUT 60)
