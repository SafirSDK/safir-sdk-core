cmake_minimum_required(VERSION 2.8)

project(lluf_startup_synchronizer_test CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FIND_PACKAGE(Boost COMPONENTS thread system)

FIND_PACKAGE(PythonInterp)

FIND_SOURCE_ROOT(SOURCE_ROOT)

ENABLE_TESTING()

ADD_SUBDIRECTORY(processes)
ADD_SUBDIRECTORY(threads)
ADD_SUBDIRECTORY(processes_and_threads)



