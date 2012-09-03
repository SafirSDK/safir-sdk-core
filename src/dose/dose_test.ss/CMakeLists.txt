cmake_minimum_required(VERSION 2.8)

#This directory contains libraries that have C interfaces, so we don't want the "d" 
#suffix on debug builds on windows. Must be set before including cmake_settings.txt
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

ADD_SUBDIRECTORY(util)
ADD_SUBDIRECTORY(cpp)
ADD_SUBDIRECTORY(dotnet)
ADD_SUBDIRECTORY(java)

if (CMAKE_SYSTEM_NAME STREQUAL "Linux")
   if (CMAKE_SYSTEM_VERSION STRGREATER "2.6.32-21-generic")
       ADD_SUBDIRECTORY(ada)      
   endif()
else ()
   ADD_SUBDIRECTORY(ada)      
endif ()



ADD_SUBDIRECTORY(Sequencer)
ADD_SUBDIRECTORY(StressTests)

INSTALL(PROGRAMS run_dose_tests.py
        DESTINATION ${SAFIR_RUNTIME}/bin)

INSTALL(DIRECTORY testcases
        DESTINATION ${SAFIR_RUNTIME}/data/text/dose_test
        PATTERN ".svn" EXCLUDE)

INSTALL(DIRECTORY output_standalone
        DESTINATION ${SAFIR_RUNTIME}/data/text/dose_test
        PATTERN ".svn" EXCLUDE)

INSTALL(DIRECTORY output_multinode
        DESTINATION ${SAFIR_RUNTIME}/data/text/dose_test
        PATTERN ".svn" EXCLUDE)

INSTALL(DIRECTORY test_parameters
        DESTINATION ${SAFIR_RUNTIME}/data/text/dose_test
        PATTERN ".svn" EXCLUDE)
