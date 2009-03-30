PROJECT (dope_main)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dope_main ${sources})

TARGET_LINK_LIBRARIES(dope_main dose_cpp swre_interface_cpp olib)

INSTALL(TARGETS dope_main
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
