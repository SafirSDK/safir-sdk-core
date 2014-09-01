cmake_minimum_required(VERSION 2.8)

project(system_picture_test_stubs CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

ADD_EXECUTABLE(control_stub sp_control.cpp)

FIND_PACKAGE(Boost COMPONENTS program_options thread system)

TARGET_LINK_LIBRARIES(control_stub
  optimized system_picture
  debug system_picture${CMAKE_DEBUG_POSTFIX}
  optimized communication
  debug communication${CMAKE_DEBUG_POSTFIX})

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(control_stub
    lluf_id
    lluf_utils
    lluf_config
    ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS control_stub
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL(PROGRAMS start_system_picture_test.py
  DESTINATION ${SAFIR_RUNTIME}/bin)
