cmake_minimum_required(VERSION 2.6)

PROJECT (dope_bin2xml)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

if (UNIX)
    FIND_PACKAGE(IODBC REQUIRED)
    INCLUDE_DIRECTORIES(${IODBC_INCLUDE_DIRS})
endif()

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dope_bin2xml ${sources})

FIND_PACKAGE(Boost COMPONENTS filesystem program_options system)

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dope_bin2xml dots_cpp dots_generated-Safir-cpp dots_generated-cpp dose_cpp olib swre_interface_cpp ${Boost_LIBRARIES} ${IODBC_LIBRARIES})
endif()

INSTALL(TARGETS dope_bin2xml
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dope_bin2xml)
