cmake_minimum_required(VERSION 2.8)
project(dose_test_sequencer CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_test_sequencer ${sources} ${headers})

#FIND_PACKAGE(Boost COMPONENTS timer program_options filesystem regex system thread)

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dose_test_sequencer PRIVATE dose_cpp dots_generated-DoseTest-cpp dots_generated-Safir-cpp dots_generated-cpp dots_cpp logging_cpp ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS dose_test_sequencer
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dose_test_sequencer)
