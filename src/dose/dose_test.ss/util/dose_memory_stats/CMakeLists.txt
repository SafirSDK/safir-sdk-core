cmake_minimum_required(VERSION 2.6)
PROJECT (dose_memory_stats)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_memory_stats ${sources})

                                                                              
FIND_PACKAGE(Boost COMPONENTS system)
TARGET_LINK_LIBRARIES(dose_memory_stats ${COMMON_LIBRARIES} ${ACE_LIBRARY})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dose_memory_stats dose_internal dots_cpp ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS dose_memory_stats
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dose_memory_stats)
