cmake_minimum_required(VERSION 2.6)

PROJECT (dose_dll)

INCLUDE_DIRECTORIES(.)

#this library has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including cmake_settings.txt
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_dll SHARED ${sources} ${headers})
TARGET_LINK_LIBRARIES(dose_dll ${ACE_LIBRARY} )

ADD_DEFINITIONS(-DDOSE_EXPORTS)

INCLUDE_DIRECTORIES( ${SAFIR_SDK}/include/Safir/Utilities/Breakpad)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(dose_dll dots_generated-cpp dose_internal breakpad)
endif()

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dose_dll
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL(FILES ../data/dose_dll.gpr
        DESTINATION ${SAFIR_SDK}/ada)

INSTALL_DEBUG_INFO(dose_dll)