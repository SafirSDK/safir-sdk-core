cmake_minimum_required(VERSION 2.8)

project(dots_cpp CXX)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dots_cpp SHARED ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS thread system)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(dots_cpp dots_kernel lluf_utils lluf_crash_reporter ${Boost_LIBRARIES})
endif()

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dots_cpp
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dots_cpp)
