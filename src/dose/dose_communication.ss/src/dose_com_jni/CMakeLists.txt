PROJECT (dose_com_jni)

INCLUDE_DIRECTORIES($ENV{JAVA_HOME}/include $ENV{JAVA_HOME}/include/linux)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

SET(CMAKE_CXX_FLAGS "-D_LINUX")

FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_com_jni SHARED ${sources} ../dose_com/DoseOsSharedMem.cpp)

INSTALL(TARGETS dose_com_jni
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
