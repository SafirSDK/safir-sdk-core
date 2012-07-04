cmake_minimum_required(VERSION 2.6)
project(VehicleAppCpp CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)
FILE(GLOB headers *.h)

ADD_EXECUTABLE(VehicleAppCpp ${sources}  ${headers})

TARGET_LINK_LIBRARIES(VehicleAppCpp ${COMMON_LIBRARIES} ${ACE_LIBRARY})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(VehicleAppCpp swre_interface_cpp dose_cpp dots_generated-Capabilities-cpp dots_generated-Safir-cpp dots_cpp lluf_utils)
endif()

INSTALL(TARGETS VehicleAppCpp
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(VehicleAppCpp)
