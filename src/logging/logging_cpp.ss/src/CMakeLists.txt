cmake_minimum_required(VERSION 2.8)

project(logging_cpp CXX)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(logging_cpp SHARED ${sources} ${headers})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(logging_cpp logging_library lluf_utils)
endif()

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS logging_cpp
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(logging_cpp)
