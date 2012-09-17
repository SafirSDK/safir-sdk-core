cmake_minimum_required(VERSION 2.6)
PROJECT (VehicleAppCpp)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)
FILE(GLOB headers *.h)

ADD_EXECUTABLE(VehicleAppCpp ${sources}  ${headers})

TARGET_LINK_LIBRARIES(VehicleAppCpp ${COMMON_LIBRARIES} ${ACE_LIBRARY})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(VehicleAppCpp  dots_generated-Capabilities-cpp dots_generated-Safir-cpp dots_generated-cpp dots_cpp lluf_utils dose_cpp swre_interface_cpp)
endif()

INSTALL(TARGETS VehicleAppCpp
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(VehicleAppCpp)
