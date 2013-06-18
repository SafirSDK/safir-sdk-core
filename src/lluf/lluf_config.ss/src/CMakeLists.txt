cmake_minimum_required(VERSION 2.6)

PROJECT (lluf_config)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(lluf_config SHARED ${sources} ${headers})

#FIND_PACKAGE(Boost COMPONENTS filesystem date_time)

#TARGET_LINK_LIBRARIES(lluf_utils ${ACE_LIBRARY} ${Boost_LIBRARIES} ${COMMON_LIBRARIES})

#ADD_DEFINITIONS(-DLLUF_UTILS_EXPORTS)

#on windows we need the psapi library
#if (WIN32)
#  TARGET_LINK_LIBRARIES(lluf_utils psapi)
#endif(WIN32)

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS lluf_config
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(lluf_config)
