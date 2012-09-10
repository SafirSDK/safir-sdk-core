cmake_minimum_required(VERSION 2.8)

project(lluf_crash_reporter_test CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FIND_PACKAGE(Boost COMPONENTS thread date_time system)

FIND_PACKAGE(PythonInterp)

ENABLE_TESTING()
ADD_SUBDIRECTORY(simple_crash_test)


