PROJECT (MessageLatencies)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp ../common/*.cpp)

ADD_EXECUTABLE(MessageLatencies ${sources})

TARGET_LINK_LIBRARIES(MessageLatencies dose_cpp boost_program_options)

INSTALL(TARGETS MessageLatencies
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
