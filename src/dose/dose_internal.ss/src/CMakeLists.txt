cmake_minimum_required(VERSION 2.8)

project(dose_internal CXX)

INCLUDE_DIRECTORIES(.)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_internal SHARED ${sources} ${headers})

FIND_PACKAGE(Boost COMPONENTS regex)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(dose_internal lluf_id lluf_utils dots_generated-Safir-cpp dots_generated-cpp dots_cpp dots_kernel lluf_startup_synchronizer ${Boost_LIBRARIES} rt)
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
