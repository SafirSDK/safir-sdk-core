cmake_minimum_required(VERSION 2.6)

project(dots_configuration_check CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dots_configuration_check ${sources})

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(dots_configuration_check dots_kernel)
endif()

INSTALL(TARGETS dots_configuration_check
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dots_configuration_check)
