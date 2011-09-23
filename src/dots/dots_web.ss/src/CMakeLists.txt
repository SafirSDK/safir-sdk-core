cmake_minimum_required(VERSION 2.6)

PROJECT (dots_web_src)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dots_web ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS filesystem system)
TARGET_LINK_LIBRARIES(dots_web ${Boost_LIBRARIES})

INSTALL(TARGETS dots_web
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        )

INSTALL_DEBUG_INFO(dots_web)
