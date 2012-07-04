cmake_minimum_required(VERSION 2.6)
project(VehicleAppDb CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

if (UNIX)
    FIND_PACKAGE(IODBC REQUIRED)
    INCLUDE_DIRECTORIES(${IODBC_INCLUDE_DIRS})
    SET(LIBS ${LIBS} ${IODBC_LIBRARIES})
endif()

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(VehicleAppDb ${sources})

TARGET_LINK_LIBRARIES(VehicleAppDb ${COMMON_LIBRARIES} ${ACE_LIBRARY} ${LIBS})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(VehicleAppDb olib swre_interface_cpp dose_cpp dots_generated-Capabilities-cpp dots_generated-Safir-cpp dots_cpp lluf_utils)
endif()

INSTALL(TARGETS VehicleAppDb
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(VehicleAppDb)
