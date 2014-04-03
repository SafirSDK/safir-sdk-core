cmake_minimum_required(VERSION 2.8)

project(system_picture_test CXX C) #C is needed by FindProtobuf

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FIND_PACKAGE(Boost COMPONENTS thread filesystem system)

FIND_PACKAGE(PythonInterp)

ENABLE_TESTING()
#ADD_SUBDIRECTORY(SharedMemory_test)


