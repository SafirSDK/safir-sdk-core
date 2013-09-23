cmake_minimum_required(VERSION 2.8)

project(lluf_startup_synchronizer_test CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FIND_PACKAGE(Boost COMPONENTS thread system)

FIND_PACKAGE(PythonInterp)

ENABLE_TESTING()

ADD_SUBDIRECTORY(processes)
ADD_SUBDIRECTORY(threads)
ADD_SUBDIRECTORY(processes_and_threads)



