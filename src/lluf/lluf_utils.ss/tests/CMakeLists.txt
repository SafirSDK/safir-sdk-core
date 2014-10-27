cmake_minimum_required(VERSION 2.8)

project(lluf_utils_test CXX)

#a library in this project library has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including safir_sdk_core_config.cmake
SET(NO_LIBRARY_POSTFIXES 1)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FIND_PACKAGE(Boost COMPONENTS timer thread filesystem program_options system unit_test_framework)

FIND_PACKAGE(PythonInterp)

ENABLE_TESTING()
ADD_SUBDIRECTORY(Ipc_test)
ADD_SUBDIRECTORY(Atomic_test)
ADD_SUBDIRECTORY(AsioPeriodicTimer_test)
ADD_SUBDIRECTORY(DynamicLibraryLoader_test)
ADD_SUBDIRECTORY(LowLevelLogger_test)
ADD_SUBDIRECTORY(ProcessInfo_test)
ADD_SUBDIRECTORY(ProcessMonitor_test)
ADD_SUBDIRECTORY(SystemLog_test)
ADD_SUBDIRECTORY(BoostSleep_test)
