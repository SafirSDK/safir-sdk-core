PROJECT (dose_dll)

INCLUDE_DIRECTORIES(.)
# INCLUDE_DIRECTORIES(/usr/include/c++/4.1.2/ext/)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_dll SHARED ${sources})
TARGET_LINK_LIBRARIES(dose_dll ACE boost_regex dots_cpp dots_generated-cpp dose_internal)

INSTALL(DIRECTORY Safir DESTINATION $ENV{SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dose_dll
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)

INSTALL(FILES ../data/dose_dll.gpr
        DESTINATION $ENV{SAFIR_SDK}/ada)
