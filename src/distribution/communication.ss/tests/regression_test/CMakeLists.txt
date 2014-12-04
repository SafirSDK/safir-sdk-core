cmake_minimum_required(VERSION 2.8)

project(regression_tests)

if (MSVC_VERSION EQUAL 1600) #vs2010 doesnt have noexcept
  add_definitions(-DSAFIR_NO_NOEXCEPT)
endif()

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FIND_PACKAGE(Boost COMPONENTS filesystem system thread chrono program_options)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(regression_test ${sources} ${headers})

TARGET_LINK_LIBRARIES(regression_test 
  optimized communication
  debug communication${CMAKE_DEBUG_POSTFIX})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(regression_test lluf_id lluf_utils ${Boost_LIBRARIES})
endif()

INSTALL(TARGETS regression_test
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(regression_test)
