cmake_minimum_required(VERSION 2.6)

PROJECT (douf_time_library)

INCLUDE_DIRECTORIES(.)

#this library has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including cmake_settings.txt
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_LIBRARY(douf_time_library SHARED ${sources})
TARGET_LINK_LIBRARIES(douf_time_library ${ACE_LIBRARY})
    
if (NOT MSVC)
    TARGET_LINK_LIBRARIES(douf_time_library dots_generated-cpp)
endif()

ADD_DEFINITIONS(-DDOUF_TIME_EXPORTS)

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS douf_time_library
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL(FILES ../../data/douf_time_library.gpr
        DESTINATION ${SAFIR_SDK}/ada)

INSTALL_DEBUG_INFO(douf_time_library)