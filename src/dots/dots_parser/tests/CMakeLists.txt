cmake_minimum_required(VERSION 2.8)

project(dots_parser_test)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FIND_PACKAGE(Boost COMPONENTS filesystem system thread)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dots_parser_test ${sources} ${headers})


if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dots_parser ${Boost_LIBRARIES})
endif()

#INSTALL(TARGETS dots_parser_test
#        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
#        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
#        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

#INSTALL_DEBUG_INFO(dots_parser_test)
