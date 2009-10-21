cmake_minimum_required(VERSION 2.6)

PROJECT (swre_logger)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(swre_logger ${sources})

TARGET_LINK_LIBRARIES(swre_logger ${ACE_LIBRARY})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(swre_logger dose_cpp douf_time_cpp)
endif()

INSTALL(TARGETS swre_logger
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)


INSTALL_DEBUG_INFO(swre_logger)