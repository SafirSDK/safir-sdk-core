cmake_minimum_required(VERSION 2.6)

PROJECT (dots_parser)

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
  TARGET_LINK_LIBRARIES(dots_parser)
endif()

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dots_parser
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dots_parser)