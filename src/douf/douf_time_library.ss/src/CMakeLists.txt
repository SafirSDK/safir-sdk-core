cmake_minimum_required(VERSION 2.8)

project(douf_time_library CXX)

INCLUDE_DIRECTORIES(.)

#this library has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including safir_sdk_core_config.cmake
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB sources *.cpp)

ADD_LIBRARY(douf_time_library SHARED ${sources})
FIND_PACKAGE(Boost COMPONENTS regex thread system)
    
if (NOT MSVC)
    TARGET_LINK_LIBRARIES(douf_time_library 
      dots_generated-douf_time-cpp 
      dots_cpp 
      ${Boost_LIBRARIES})
endif()

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS douf_time_library
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL(FILES ../data/douf_time_library.gpr
        DESTINATION ${SAFIR_SDK}/ada)

INSTALL_DEBUG_INFO(douf_time_library)
