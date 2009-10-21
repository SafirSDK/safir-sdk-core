cmake_minimum_required(VERSION 2.6)
PROJECT (VehicleAppCpp)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(VehicleAppCpp ${sources})

TARGET_LINK_LIBRARIES(VehicleAppCpp ${COMMON_LIBRARIES} ${ACE_LIBRARY})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(VehicleAppCpp dose_cpp swre_interface_cpp)
endif()

INSTALL(TARGETS VehicleAppCpp
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(VehicleAppCpp)