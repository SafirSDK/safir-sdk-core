cmake_minimum_required(VERSION 2.8)

project(logging_library CXX)

INCLUDE_DIRECTORIES(.)

#this library has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including safir_sdk_core_config.cmake
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(logging_library SHARED ${sources} ${headers})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(logging_library lluf_utils)
endif()

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS logging_library
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL(FILES ../data/logging_library.gpr
        DESTINATION ${SAFIR_SDK}/ada)


INSTALL_DEBUG_INFO(logging_library)
