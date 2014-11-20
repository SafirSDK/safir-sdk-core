cmake_minimum_required(VERSION 2.8)

project(control CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

ADD_SUBDIRECTORY(boost-process)

INCLUDE_DIRECTORIES(. boost-process/src)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(safir_control ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS program_options thread system)

TARGET_LINK_LIBRARIES(safir_control
  optimized system_picture
  debug system_picture${CMAKE_DEBUG_POSTFIX}
  optimized communication
  debug communication${CMAKE_DEBUG_POSTFIX}
  optimized control_utils
  debug control_utils${CMAKE_DEBUG_POSTFIX})

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(safir_control
    dots_internal   
    lluf_id
    lluf_utils
    lluf_config
    ${Boost_LIBRARIES})
endif()

add_dependencies(safir_control boost-process)

INSTALL(TARGETS safir_control
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(safir_control)
