cmake_minimum_required(VERSION 2.8)

project(communication_test)

if (MSVC_VERSION EQUAL 1600) #vs2010 doesnt have noexcept
  add_definitions(-DSAFIR_NO_NOEXCEPT)
endif()

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FIND_PACKAGE(Boost COMPONENTS filesystem system thread chrono program_options)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(communication_test ${sources} ${headers})

TARGET_LINK_LIBRARIES(communication_test communication)

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(communication_test lluf_id lluf_utils ${Boost_LIBRARIES})
endif()

