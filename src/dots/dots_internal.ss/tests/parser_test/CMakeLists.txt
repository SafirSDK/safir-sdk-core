FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
ADD_EXECUTABLE(dots_parser_test ${sources} ${headers})

TARGET_LINK_LIBRARIES(dots_parser_test PRIVATE 
  lluf_config
  lluf_id 
  dots_internal
  ${Boost_FILESYSTEM_LIBRARY})

add_test(dots_parser_test_cases dots_parser_test ${CMAKE_CURRENT_SOURCE_DIR}/dou_test_files)
SET_SAFIR_TEST_PROPERTIES(TEST dots_parser_test_cases)

