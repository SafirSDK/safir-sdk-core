PROJECT (dose_test_sequencer)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_test_sequencer ${sources})

TARGET_LINK_LIBRARIES(dose_test_sequencer dots_generated-cpp boost_filesystem dose_cpp boost_program_options)

INSTALL(TARGETS dose_test_sequencer
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
