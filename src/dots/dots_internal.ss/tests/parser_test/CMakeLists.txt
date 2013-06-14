cmake_minimum_required(VERSION 2.8)

project(dots_parser_test)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FIND_PACKAGE(Boost COMPONENTS filesystem system thread)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dots_parser_test ${sources} ${headers})


if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dots_parser_test dots_internal ${Boost_LIBRARIES})
endif()

enable_testing()

add_test(dots_parser_test_cases dots_parser_test ${CMAKE_CURRENT_SOURCE_DIR}/dou_test_files)
