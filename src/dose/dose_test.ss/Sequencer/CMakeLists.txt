cmake_minimum_required(VERSION 2.6)
PROJECT (dose_test_sequencer)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_test_sequencer ${sources})

TARGET_LINK_LIBRARIES(dose_test_sequencer ${COMMON_LIBRARIES} ${ACE_LIBRARY})

if (NOT MSVC)
    FIND_PACKAGE(Boost COMPONENTS program_options filesystem)
    TARGET_LINK_LIBRARIES(dose_test_sequencer dots_generated-cpp dose_cpp ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS dose_test_sequencer
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dose_test_sequencer)