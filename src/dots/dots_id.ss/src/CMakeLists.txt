cmake_minimum_required(VERSION 2.8)

project(dots_id C CXX)

INCLUDE_DIRECTORIES(.)

#this library has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including cmake_settings.txt
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

ADD_LIBRARY(dots_id SHARED dots_id.cpp md5.c md5.h Safir/Dob/Typesystem/Internal/Id.h)

FIND_PACKAGE(Boost COMPONENTS filesystem date_time thread)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(dots_id lluf_utils ${Boost_LIBRARIES})
endif()

ADD_DEFINITIONS(-DDOTS_ID_EXPORTS)

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dots_id
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL(FILES ../data/dots_id.gpr
        DESTINATION ${SAFIR_SDK}/ada/)

INSTALL_DEBUG_INFO(dots_id)
