cmake_minimum_required(VERSION 2.8)

project(dose_main_stub CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_main_stub ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS program_options thread system)

TARGET_LINK_LIBRARIES(dose_main_stub
  optimized system_picture
  debug system_picture${CMAKE_DEBUG_POSTFIX}
  optimized communication
  debug communication${CMAKE_DEBUG_POSTFIX})

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(dose_main_stub
    dots_internal
    lluf_id
    lluf_utils
    lluf_config
    ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS dose_main_stub
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dose_main_stub)
