cmake_minimum_required(VERSION 2.6)

PROJECT (swre_logger)

if (MSVC)
    ADD_DEFINITIONS(-D_WIN32_WINNT=0x0600)
    ADD_DEFINITIONS(-DWIN32_LEAN_AND_MEAN)
endif()

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(swre_logger ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS regex filesystem system date_time)

TARGET_LINK_LIBRARIES(swre_logger ${ACE_LIBRARY})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(swre_logger dots_cpp dots_generated-Safir-cpp dots_generated-cpp dose_cpp douf_time_cpp lluf_config ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS swre_logger
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)


INSTALL_DEBUG_INFO(swre_logger)
