cmake_minimum_required(VERSION 2.8)

project(dots_kernel CXX)

INCLUDE_DIRECTORIES(.)

#this library has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including safir_sdk_core_config.cmake
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dots_kernel SHARED ${sources} ${headers})

find_library(EXPAT_LIBRARY NAMES expat libexpat
             HINTS ${SAFIR_SDK}/lib
             PATHS ENV PATH
             PATH_SUFFIXES ../lib)
if (NOT EXPAT_LIBRARY)
    message(FATAL_ERROR "Couldn't find expat.lib or libexpat.lib")
endif()

#on windows we don't trust that expat is installed on the INCLUDE path.
#we also check where it ends up if you use the expat install package.
if (WIN32)
    find_path(EXPAT_HEADER NAMES expat.h
              HINTS ${SAFIR_SDK}/include
              PATHS ENV PATH
              PATH_SUFFIXES ../Source/lib ../include)
    if (NOT EXPAT_HEADER)
        message(FATAL_ERROR "Couldn't find expat.h")
    endif()
    
    INCLUDE_DIRECTORIES(${EXPAT_HEADER})
endif()

FIND_PACKAGE(Boost COMPONENTS regex filesystem date_time system)

TARGET_LINK_LIBRARIES(dots_kernel ${EXPAT_LIBRARY})

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES(dots_kernel dots_internal lluf_utils lluf_startup_synchronizer lluf_crash_reporter lluf_config ${Boost_LIBRARIES} rt)
ENDIF()

ADD_DEFINITIONS(-DDOTSKERNEL_EXPORTS)

#check if we can do unaligned accesses, or if we should define a symbol that fixes it.
include(CheckCXXSourceRuns)
SET(CMAKE_REQUIRED_FLAGS ${CMAKE_REQUIRED_FLAGS} -O2)
CHECK_CXX_SOURCE_RUNS("#include \"${CMAKE_CURRENT_SOURCE_DIR}/checks/check_unaligned_access.cpp\"" can_do_unaligned_access)
if(NOT can_do_unaligned_access)
  MESSAGE("This platform does not appear able to do unaligned accesses, so we'll use memcpy instead")
  ADD_DEFINITIONS(-DNO_UNALIGNED_ACCESS)
endif()


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
