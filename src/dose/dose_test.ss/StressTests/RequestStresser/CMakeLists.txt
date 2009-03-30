PROJECT (RequestStresser)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp ../common/*.cpp)

ADD_EXECUTABLE(RequestStresser ${sources})

TARGET_LINK_LIBRARIES(RequestStresser dose_cpp boost_program_options)

INSTALL(TARGETS RequestStresser
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
