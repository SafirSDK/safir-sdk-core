cmake_minimum_required(VERSION 2.6)

project(douf_time_cpp CXX)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(douf_time_cpp SHARED ${sources} ${headers})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(douf_time_cpp douf_time_library dots_generated-cpp)
endif()

ADD_DEFINITIONS(-DDOUF_TIME_CPP_EXPORTS)

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS douf_time_cpp
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(douf_time_cpp)
