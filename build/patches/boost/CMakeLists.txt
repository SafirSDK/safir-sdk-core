project(ubuntu_patches)

cmake_minimum_required(VERSION 2.4)

find_package(Boost)

add_subdirectory("${Boost_LIB_VERSION}")
