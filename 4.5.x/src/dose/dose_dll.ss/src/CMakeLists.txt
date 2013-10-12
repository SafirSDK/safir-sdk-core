cmake_minimum_required(VERSION 2.8)

project(dose_dll CXX)

INCLUDE_DIRECTORIES(.)

#this library has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including safir_sdk_core_config.cmake
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

FIND_PACKAGE(Boost COMPONENTS thread)

ADD_LIBRARY(dose_dll SHARED ${sources} ${headers})

ADD_DEFINITIONS(-DDOSE_EXPORTS)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(dose_dll dose_internal ${Boost_LIBRARIES})
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
