cmake_minimum_required(VERSION 3.27)

PROJECT (dose_cpp)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

find_package(Boost COMPONENTS filesystem)
ADD_LIBRARY(dose_cpp SHARED ${sources} ${headers})
TARGET_LINK_LIBRARIES(dose_cpp ${Boost_LIBRARIES})

ADD_DEFINITIONS(-DDOSE_CPP_EXPORTS)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(dose_cpp dots_generated-cpp dose_dll)
endif()

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dose_cpp
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dose_cpp)