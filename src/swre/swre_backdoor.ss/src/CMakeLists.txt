PROJECT (swre_backdoor)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(backdoor ${sources})

TARGET_LINK_LIBRARIES(backdoor ACE dose_cpp boost_program_options)

INSTALL(TARGETS backdoor
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
