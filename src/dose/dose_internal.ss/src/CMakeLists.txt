PROJECT (dose_internal)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_internal SHARED ${sources})
TARGET_LINK_LIBRARIES(dose_internal ACE dots_kernel dots_generated-cpp)

INSTALL(DIRECTORY Safir DESTINATION $ENV{SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dose_internal
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
