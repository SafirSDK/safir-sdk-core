cmake_minimum_required(VERSION 2.6)

project(dose_internal CXX)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_internal SHARED ${sources} ${headers})

ADD_DEFINITIONS(-DDOSE_INT_EXPORTS)

FIND_PACKAGE(Boost COMPONENTS regex)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(dose_internal dots_generated-Safir-cpp dots_generated-cpp dots_cpp dots_kernel ${Boost_LIBRARIES})
endif()

INSTALL(DIRECTORY Safir 
  DESTINATION ${SAFIR_SDK}/include
  PATTERN ".svn" EXCLUDE
  PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dose_internal
  RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
  LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
  ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dose_internal)
