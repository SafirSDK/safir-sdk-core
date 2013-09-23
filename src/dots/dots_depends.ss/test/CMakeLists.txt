cmake_minimum_required(VERSION 2.8)

project(dots_depends_test CXX)

add_subdirectory(generate_max_dots)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

INSTALL(FILES 
        data/DotsMax.Entity.dou
        DESTINATION ${SAFIR_SDK}/dots/dots_generated)
