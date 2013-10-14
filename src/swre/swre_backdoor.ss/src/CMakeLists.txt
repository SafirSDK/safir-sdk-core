cmake_minimum_required(VERSION 2.8)

project(swre_backdoor CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(backdoor ${sources})

FIND_PACKAGE(Boost COMPONENTS program_options system)
TARGET_LINK_LIBRARIES(backdoor ${Boost_LIBRARIES})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(backdoor dots_cpp dots_generated-Safir-cpp dots_generated-cpp dose_cpp)
endif()

INSTALL(TARGETS backdoor
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)


INSTALL_DEBUG_INFO(backdoor)
