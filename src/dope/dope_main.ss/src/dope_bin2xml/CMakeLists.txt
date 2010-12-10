cmake_minimum_required(VERSION 2.6)

PROJECT (dope_bin2xml)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

if (UNIX)
    FIND_PACKAGE(IODBC REQUIRED)
    INCLUDE_DIRECTORIES(${IODBC_INCLUDE_DIRS})
endif()

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dope_bin2xml ${sources})

FIND_PACKAGE(Boost COMPONENTS program_options)

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dope_bin2xml dots_cpp swre_interface_cpp olib ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS dope_bin2xml
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dope_bin2xml)
