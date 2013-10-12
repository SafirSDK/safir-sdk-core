cmake_minimum_required(VERSION 2.8)

project(generate_max_dots CXX)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(generate_max_dots ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS filesystem )

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(generate_max_dots ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS generate_max_dots
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        )

INSTALL_DEBUG_INFO(generate_max_dots)
