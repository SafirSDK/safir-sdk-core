cmake_minimum_required(VERSION 2.8)

PROJECT (lluf_utils CXX)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(lluf_utils SHARED ${sources} ${headers})

if (Boost_VERSION LESS 105100) #less than 1.41 then we need program options
  FIND_PACKAGE(Boost COMPONENTS filesystem date_time system thread program_options)
else()
  FIND_PACKAGE(Boost COMPONENTS filesystem date_time system thread)
endif()
TARGET_LINK_LIBRARIES(lluf_utils ${COMMON_LIBRARIES})

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(lluf_utils lluf_startup_synchronizer ${Boost_LIBRARIES} dl)
endif()

ADD_DEFINITIONS(-DLLUF_UTILS_EXPORTS)

#on windows we need the psapi library
if (WIN32)
  TARGET_LINK_LIBRARIES(lluf_utils psapi)
endif(WIN32)

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS lluf_utils 
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL(FILES  
        ../data/logging.ini
        DESTINATION ${SAFIR_RUNTIME}/log/)

INSTALL_DEBUG_INFO(lluf_utils)
