cmake_minimum_required(VERSION 2.8)

project(dots_web CXX)

add_subdirectory(src)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

INSTALL(DIRECTORY web
        DESTINATION ${SAFIR_RUNTIME}/data/text
        PATTERN ".svn" EXCLUDE)
