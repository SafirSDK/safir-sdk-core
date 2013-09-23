cmake_minimum_required(VERSION 2.8)

project(dots_web_src CXX)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dots_web ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS filesystem system)
if (NOT MSVC)
  TARGET_LINK_LIBRARIES(dots_web ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS dots_web
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        )

INSTALL_DEBUG_INFO(dots_web)
