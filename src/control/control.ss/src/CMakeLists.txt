cmake_minimum_required(VERSION 2.8)

project(control CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(safir_control ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS program_options thread system)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(safir_control
    system_picture
    communication
    lluf_id
    lluf_utils
    ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS safir_control
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(safir_control)
