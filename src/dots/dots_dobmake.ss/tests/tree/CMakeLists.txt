cmake_minimum_required(VERSION 3.11)

project(dobmake_test_tree CXX)

find_package(SafirSDKCore REQUIRED)

add_subdirectory(dous_1)
add_subdirectory(dous_2)
add_subdirectory(dous_3)
