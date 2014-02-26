cmake_minimum_required(VERSION 2.8)

project(lluf_id C CXX)

INCLUDE_DIRECTORIES(.)

#this library has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including safir_sdk_core_config.cmake
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

ADD_LIBRARY(lluf_id SHARED lluf_id.cpp md5.c md5.h Safir/Utilities/Internal/Id.h)

FIND_PACKAGE(Boost COMPONENTS filesystem thread)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(lluf_id lluf_utils ${Boost_LIBRARIES})
endif()

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS lluf_id
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL(FILES ../data/lluf_id.gpr
        DESTINATION ${SAFIR_SDK}/ada/)

INSTALL_DEBUG_INFO(lluf_id)
