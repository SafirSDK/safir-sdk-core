PROJECT (swre_interface_cpp)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_LIBRARY(swre_interface_cpp SHARED ${sources})
TARGET_LINK_LIBRARIES(swre_interface_cpp dose_cpp swre_library douf_time)

INSTALL(DIRECTORY Safir DESTINATION $ENV{SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS swre_interface_cpp
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
