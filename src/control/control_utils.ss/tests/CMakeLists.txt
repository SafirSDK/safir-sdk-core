cmake_minimum_required(VERSION 2.8)

project(control_utils_test CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FIND_PACKAGE(Boost COMPONENTS timer thread filesystem program_options system)

FIND_PACKAGE(PythonInterp)

ENABLE_TESTING()
ADD_SUBDIRECTORY(unit_tests/Config_test)


