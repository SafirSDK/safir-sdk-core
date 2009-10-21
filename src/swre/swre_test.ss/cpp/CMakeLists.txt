PROJECT (swre_test)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(swre_test ${sources})

TARGET_LINK_LIBRARIES(swre_test dose_cpp swre_interface_cpp douf_time_cpp)

INSTALL(TARGETS swre_test
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
