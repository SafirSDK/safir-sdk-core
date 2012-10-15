cmake_minimum_required(VERSION 2.8)

project(swre_library CXX)

INCLUDE_DIRECTORIES(.)

#this library has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including cmake_settings.txt
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(swre_library SHARED ${sources} ${headers})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(swre_library dose_cpp dots_generated-cpp douf_time_cpp lluf_utils lluf_crash_reporter)
endif()

ADD_DEFINITIONS(-DSWRE_LIBRARY_EXPORTS)

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS swre_library
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL(FILES ../data/swre_library.gpr
        DESTINATION ${SAFIR_SDK}/ada)


INSTALL_DEBUG_INFO(swre_library)
