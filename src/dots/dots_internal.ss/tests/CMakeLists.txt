cmake_minimum_required(VERSION 2.8)

project(dots_internal_tests CXX)

#INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)
INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FIND_PACKAGE(Boost COMPONENTS filesystem system thread)

ENABLE_TESTING()
ADD_SUBDIRECTORY(parser_test)
ADD_SUBDIRECTORY(serialization_test)
ADD_SUBDIRECTORY(shmem_repo)
ADD_SUBDIRECTORY(override_test)
