cmake_minimum_required(VERSION 2.8)

PROJECT (dots_parser CXX)

if (MSVC_VERSION EQUAL 1600) #vs2010 doesnt have noexcept
  add_definitions(-DSAFIR_NO_NOEXCEPT)
endif()

if (NOT MSVC)
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
endif()

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dots_parser SHARED ${sources} ${headers})
TARGET_LINK_LIBRARIES(dots_parser)

FIND_PACKAGE(Boost COMPONENTS regex filesystem date_time)
TARGET_LINK_LIBRARIES(dots_parser ${Boost_LIBRARIES} ${COMMON_LIBRARIES})


ADD_DEFINITIONS(-DDOTS_PARSER_EXPORTS)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(dots_parser dots_id lluf_utils)
endif()

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dots_parser
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dots_parser)
