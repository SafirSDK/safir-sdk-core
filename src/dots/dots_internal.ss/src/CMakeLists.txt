cmake_minimum_required(VERSION 2.8)

PROJECT (dots_internal CXX)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

#if we're using a very old GCC we need to disable some spurious warnings.
if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.5)
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-array-bounds -Wno-strict-aliasing")
endif()

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dots_internal SHARED ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS regex filesystem)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(dots_internal dots_id lluf_utils  ${Boost_LIBRARIES})
endif()

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dots_internal
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dots_internal)
