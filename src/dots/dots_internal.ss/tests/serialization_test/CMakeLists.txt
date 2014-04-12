FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
ADD_EXECUTABLE(dots_serialization_test ${sources} ${headers})

TARGET_LINK_LIBRARIES(dots_serialization_test PRIVATE 
  lluf_config
  lluf_id 
  dots_internal
  ${Boost_FILESYSTEM_LIBRARY})

add_test(dots_serialization_test_cases dots_serialization_test ${CMAKE_CURRENT_SOURCE_DIR}/dou ${CMAKE_CURRENT_SOURCE_DIR}/testcases)
SET_SAFIR_TEST_PROPERTIES(TEST dots_serialization_test_cases)

