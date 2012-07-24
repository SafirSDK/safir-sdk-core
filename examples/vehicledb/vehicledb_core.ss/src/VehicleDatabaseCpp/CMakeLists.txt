cmake_minimum_required(VERSION 2.6)
project(VehicleAppDb CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)
FILE(GLOB headers *.h)

ADD_EXECUTABLE(VehicleAppDb ${sources} ${headers})

find_package(ACE)

if (ACE_FOUND)
  INCLUDE_DIRECTORIES(${ACE_INCLUDE_DIRECTORIES})
  TARGET_LINK_LIBRARIES(VehicleAppDb ${ACE_LIBRARY})
else()
  ADD_DEFINITIONS(-DNO_ACE)
  FIND_PACKAGE(Boost COMPONENTS system)
  TARGET_LINK_LIBRARIES(VehicleAppDb ${Boost_LIBRARIES})
endif()

if (UNIX)
  FIND_PACKAGE(IODBC)
  if (IODBC_FOUND)
    INCLUDE_DIRECTORIES(${IODBC_INCLUDE_DIRS})
    SET(ODBC_LIBRARIES ${IODBC_LIBRARIES})
    message("iODBC not found, trying UnixODBC")
  else()
    FIND_PACKAGE(UnixODBC REQUIRED)
    INCLUDE_DIRECTORIES(${UnixODBC_INCLUDE_DIRS})
    SET(ODBC_LIBRARIES ${UnixODBC_LIBRARIES})
  endif()
endif()
  
TARGET_LINK_LIBRARIES(VehicleAppDb ${COMMON_LIBRARIES} ${ODBC_LIBRARIES})
  
if (NOT MSVC)
  TARGET_LINK_LIBRARIES(VehicleAppDb olib swre_interface_cpp dose_cpp dots_generated-Capabilities-cpp dots_generated-Safir-cpp dots_cpp lluf_utils)
endif()
  
INSTALL(TARGETS VehicleAppDb
  RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
  LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
  ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(VehicleAppDb)

