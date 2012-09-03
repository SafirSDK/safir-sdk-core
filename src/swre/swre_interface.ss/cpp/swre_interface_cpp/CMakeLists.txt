cmake_minimum_required(VERSION 2.8)

project(swre_interface_cpp CXX)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(swre_interface_cpp SHARED ${sources} ${headers})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(swre_interface_cpp dose_cpp swre_library douf_time_cpp)
endif()

ADD_DEFINITIONS(-DSWRE_EXPORTS)

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS swre_interface_cpp
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(swre_interface_cpp)
