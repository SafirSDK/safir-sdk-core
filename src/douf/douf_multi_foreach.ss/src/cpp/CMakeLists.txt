cmake_minimum_required(VERSION 2.6)

PROJECT (douf_foreach)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(foreach ${sources} ${headers})

TARGET_LINK_LIBRARIES(foreach ${ACE_LIBRARY})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(foreach swre_interface_cpp dose_cpp)
endif()

INSTALL(TARGETS foreach
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(foreach)