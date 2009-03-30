PROJECT (douf_time_library)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_LIBRARY(douf_time_library SHARED ${sources})
TARGET_LINK_LIBRARIES(douf_time_library dots_generated-cpp)

INSTALL(DIRECTORY Safir DESTINATION $ENV{SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS douf_time_library
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)

INSTALL(FILES ../../data/douf_time_library.gpr
        DESTINATION $ENV{SAFIR_SDK}/ada)
