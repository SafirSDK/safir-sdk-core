cmake_minimum_required(VERSION 2.8)
project(VehicleAppCpp CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

FILE(GLOB sources *.cpp)
FILE(GLOB headers *.h)

ADD_EXECUTABLE(VehicleAppCpp ${sources} ${headers})


find_package(ACE)

if (ACE_FOUND)
  INCLUDE_DIRECTORIES(${ACE_INCLUDE_DIRECTORIES})
  TARGET_LINK_LIBRARIES(VehicleAppCpp ${ACE_LIBRARY})
else()
  ADD_DEFINITIONS(-DNO_ACE)
  FIND_PACKAGE(Boost COMPONENTS system)
  TARGET_LINK_LIBRARIES(VehicleAppCpp ${Boost_LIBRARIES})
endif()
  
if (NOT MSVC)
  TARGET_LINK_LIBRARIES(VehicleAppCpp swre_interface_cpp dose_cpp dots_generated-Capabilities-cpp dots_generated-Safir-cpp dots_cpp logging_cpp lluf_utils)
endif()

INSTALL(TARGETS VehicleAppCpp
  RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
  LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
  ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(VehicleAppCpp)
