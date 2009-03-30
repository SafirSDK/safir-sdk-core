PROJECT (swre_library)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_LIBRARY(swre_library SHARED ${sources})
TARGET_LINK_LIBRARIES(swre_library dose_cpp ACE dots_generated-cpp douf_time)

INSTALL(DIRECTORY Safir DESTINATION $ENV{SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS swre_library
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)

INSTALL(FILES ../data/swre_library.gpr
        DESTINATION $ENV{SAFIR_SDK}/ada)
