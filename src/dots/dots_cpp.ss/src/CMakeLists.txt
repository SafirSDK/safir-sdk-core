PROJECT (dots_cpp)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

SET(CMAKE_CXX_FLAGS "-Wall -Wunused-parameter")

ADD_LIBRARY(dots_cpp SHARED ${sources})
TARGET_LINK_LIBRARIES(dots_cpp dots_kernel ACE lluf_utils)

INSTALL(DIRECTORY Safir DESTINATION $ENV{SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dots_cpp
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
