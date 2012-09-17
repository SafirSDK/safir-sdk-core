cmake_minimum_required(VERSION 2.6)

PROJECT (dope_main)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

if (UNIX)
    FIND_PACKAGE(IODBC REQUIRED)
    INCLUDE_DIRECTORIES(${IODBC_INCLUDE_DIRS})
endif()

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dope_main ${sources} ${headers})
FIND_PACKAGE(Boost COMPONENTS filesystem system)
TARGET_LINK_LIBRARIES(dope_main ${ACE_LIBRARY})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dope_main dots_cpp dots_generated-Safir-cpp dots_generated-cpp dose_cpp swre_interface_cpp olib ${Boost_LIBRARIES} ${IODBC_LIBRARIES})
endif()

INSTALL(TARGETS dope_main
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)


INSTALL_DEBUG_INFO(dope_main)
