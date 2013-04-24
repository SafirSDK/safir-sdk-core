cmake_minimum_required(VERSION 2.8)
project(dose_memory_stats CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_memory_stats ${sources})

FIND_PACKAGE(Boost COMPONENTS system thread)

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dose_memory_stats dose_internal dots_cpp ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS dose_memory_stats
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dose_memory_stats)
