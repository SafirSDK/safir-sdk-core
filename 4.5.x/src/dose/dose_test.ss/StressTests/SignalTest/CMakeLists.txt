cmake_minimum_required(VERSION 2.8)
project(SignalTest CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB sources *.cpp ../common/*.cpp)
FILE(GLOB headers *.h ../common/*.h)

ADD_EXECUTABLE(SignalTest ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS program_options)

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(SignalTest dose_internal ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS SignalTest
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)


INSTALL_DEBUG_INFO(SignalTest)
