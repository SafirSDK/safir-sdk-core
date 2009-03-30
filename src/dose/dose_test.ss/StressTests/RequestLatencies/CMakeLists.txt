PROJECT (RequestLatencies)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp ../common/*.cpp)

ADD_EXECUTABLE(RequestLatencies ${sources})

TARGET_LINK_LIBRARIES(RequestLatencies dose_cpp boost_program_options)

INSTALL(TARGETS RequestLatencies
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
