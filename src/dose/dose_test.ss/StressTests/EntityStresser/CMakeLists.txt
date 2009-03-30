PROJECT (EntityStresser)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp ../common/*.cpp)

ADD_EXECUTABLE(EntityStresser ${sources})

TARGET_LINK_LIBRARIES(EntityStresser dose_cpp boost_program_options)

INSTALL(TARGETS EntityStresser
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
