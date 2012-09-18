cmake_minimum_required(VERSION 2.6)

PROJECT (swre_test)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(swre_test ${sources} ${headers})

TARGET_LINK_LIBRARIES(swre_test ${ACE_LIBRARY})

IF (NOT MSVC)
   TARGET_LINK_LIBRARIES(swre_test dots_generated-Safir-cpp dose_cpp swre_interface_cpp dots_cpp douf_time_cpp lluf_utils)
ENDIF ()

INSTALL(TARGETS swre_test
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
