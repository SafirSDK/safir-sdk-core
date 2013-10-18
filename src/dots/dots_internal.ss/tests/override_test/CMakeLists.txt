FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
ADD_EXECUTABLE(dots_override_test ${sources} ${headers})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dots_override_test dots_id dots_internal ${Boost_LIBRARIES})
endif()

add_test(dots_override_test_1 dots_override_test ${CMAKE_CURRENT_SOURCE_DIR}/test/test1.txt)
add_test(dots_override_test_2 dots_override_test ${CMAKE_CURRENT_SOURCE_DIR}/test/test2.txt)
add_test(dots_override_test_3 dots_override_test ${CMAKE_CURRENT_SOURCE_DIR}/test/test3.txt)
add_test(dots_override_test_4 dots_override_test ${CMAKE_CURRENT_SOURCE_DIR}/test/test4.txt)
add_test(dots_override_test_5 dots_override_test ${CMAKE_CURRENT_SOURCE_DIR}/test/test5.txt)
add_test(dots_override_test_6 dots_override_test ${CMAKE_CURRENT_SOURCE_DIR}/test/test6.txt)
