cmake_minimum_required(VERSION 2.8)

project(dots_configuration_check CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dots_configuration_check ${sources})

FIND_PACKAGE(Boost COMPONENTS regex filesystem date_time system program_options)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(dots_configuration_check dots_internal dots_kernel ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS dots_configuration_check
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dots_configuration_check)
