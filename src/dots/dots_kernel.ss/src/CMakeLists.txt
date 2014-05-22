cmake_minimum_required(VERSION 2.8)

project(dots_kernel CXX)

INCLUDE_DIRECTORIES(.)

#this library has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including safir_sdk_core_config.cmake
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

#if we're using a very old GCC we need to disable some spurious warnings.
if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.5)
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-array-bounds -Wno-strict-aliasing")
endif()

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dots_kernel SHARED ${sources} ${headers})


FIND_PACKAGE(Boost COMPONENTS regex filesystem system)

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES(dots_kernel dots_internal lluf_utils lluf_startup_synchronizer lluf_crash_reporter lluf_config ${Boost_LIBRARIES} rt)
ENDIF()

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dots_kernel
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL(FILES ../data/dots_kernel.gpr
        DESTINATION ${SAFIR_SDK}/ada/)

INSTALL_DEBUG_INFO(dots_kernel)
