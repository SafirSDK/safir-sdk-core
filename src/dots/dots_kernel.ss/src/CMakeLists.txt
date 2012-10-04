cmake_minimum_required(VERSION 2.8)

project(dots_kernel CXX)

INCLUDE_DIRECTORIES(.)

#this library has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including cmake_settings.txt
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

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

FIND_PACKAGE(Boost COMPONENTS regex filesystem date_time)

TARGET_LINK_LIBRARIES(dots_kernel ${COMMON_LIBRARIES} ${EXPAT_LIBRARY})

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES(dots_kernel dots_id lluf_utils ${Boost_LIBRARIES})
ENDIF()

ADD_DEFINITIONS(-DDOTSKERNEL_EXPORTS)

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
