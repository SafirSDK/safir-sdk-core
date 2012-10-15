cmake_minimum_required(VERSION 2.8)

project(swre_logger CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(swre_logger ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS regex filesystem system date_time)

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(swre_logger dots_cpp dots_generated-Safir-cpp dots_generated-cpp dose_cpp douf_time_cpp ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS swre_logger
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)


INSTALL_DEBUG_INFO(swre_logger)
