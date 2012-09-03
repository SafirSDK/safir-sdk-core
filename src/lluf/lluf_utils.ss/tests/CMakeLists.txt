cmake_minimum_required(VERSION 2.8)

project(lluf_utils_test CXX)

#a library in this project library has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including cmake_settings.txt
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FIND_PACKAGE(Boost COMPONENTS thread date_time system)

FIND_PACKAGE(PythonInterp)

ENABLE_TESTING()
ADD_SUBDIRECTORY(DynamicLibraryLoader_test)
ADD_SUBDIRECTORY(LowLevelLogger_test)
ADD_SUBDIRECTORY(ProcessInfo_test)
ADD_SUBDIRECTORY(ProcessMonitor_test)
ADD_SUBDIRECTORY(StartupSynchronizer_test)
ADD_SUBDIRECTORY(PanicLogging_test)


