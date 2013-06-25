cmake_minimum_required(VERSION 2.8)

project(dots_internal_tests CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FIND_PACKAGE(Boost COMPONENTS filesystem system thread)

ENABLE_TESTING()
ADD_SUBDIRECTORY(parser_test)
ADD_SUBDIRECTORY(serialization_test)

