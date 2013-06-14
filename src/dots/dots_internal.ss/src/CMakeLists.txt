cmake_minimum_required(VERSION 2.8)

PROJECT (dots_internal CXX)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dots_internal SHARED ${sources} ${headers})
TARGET_LINK_LIBRARIES(dots_internal)

FIND_PACKAGE(Boost COMPONENTS regex filesystem date_time)
TARGET_LINK_LIBRARIES(dots_internal ${Boost_LIBRARIES} ${COMMON_LIBRARIES})


ADD_DEFINITIONS(-DDOTS_INTERNAL_EXPORTS)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(dots_internal dots_id lluf_utils)
endif()

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dots_internal
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dots_internal)
