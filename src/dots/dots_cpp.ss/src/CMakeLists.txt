cmake_minimum_required(VERSION 2.6)

PROJECT (dots_cpp)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dots_cpp SHARED ${sources} ${headers})
TARGET_LINK_LIBRARIES(dots_cpp ${ACE_LIBRARY})

ADD_DEFINITIONS(-DDOTS_EXPORTS)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(dots_cpp dots_kernel lluf_utils)
endif()

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dots_cpp
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dots_cpp)