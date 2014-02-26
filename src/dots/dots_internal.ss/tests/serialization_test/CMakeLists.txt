FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
ADD_EXECUTABLE(dots_serialization_test ${sources} ${headers})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dots_serialization_test lluf_id dots_internal ${Boost_LIBRARIES})
endif()

add_test(dots_serialization_test_cases dots_serialization_test ${CMAKE_CURRENT_SOURCE_DIR}/dou ${CMAKE_CURRENT_SOURCE_DIR}/testcases)
SET_SAFIR_TEST_PROPERTIES(dots_serialization_test_cases)

