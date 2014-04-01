cmake_minimum_required(VERSION 2.8)

PROJECT(safir_sdk_core CXX C)

include(../build/config/sdk/data/build/safir_sdk_core_config.cmake)

include_directories(lluf/lluf_build_helpers.ss/src)

ADD_DEFINITIONS(-DSAFIR_NO_AUTOLINK)

add_subdirectory(lluf)
