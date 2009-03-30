PROJECT (dope_bin2xml)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dope_bin2xml ${sources})

TARGET_LINK_LIBRARIES(dope_bin2xml dots_cpp boost_program_options swre_interface_cpp olib)

INSTALL(TARGETS dope_bin2xml
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
