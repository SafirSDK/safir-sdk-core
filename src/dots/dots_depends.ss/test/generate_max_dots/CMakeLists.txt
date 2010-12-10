cmake_minimum_required(VERSION 2.6)

PROJECT (generate_max_dots)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(generate_max_dots ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS filesystem )
TARGET_LINK_LIBRARIES(generate_max_dots ${Boost_LIBRARIES})

INSTALL(TARGETS generate_max_dots
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        )

INSTALL_DEBUG_INFO(generate_max_dots)
