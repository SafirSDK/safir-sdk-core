#TODO
#a library in this project library has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including safir_sdk_core_config.cmake
SET(NO_LIBRARY_POSTFIXES 1) 

ADD_SUBDIRECTORY(DynamicLibraryLoader_test)
ADD_SUBDIRECTORY(LowLevelLogger_test)
ADD_SUBDIRECTORY(ProcessInfo_test)
ADD_SUBDIRECTORY(ProcessMonitor_test)
ADD_SUBDIRECTORY(SystemLog_test)
ADD_SUBDIRECTORY(BoostSleep_test)

