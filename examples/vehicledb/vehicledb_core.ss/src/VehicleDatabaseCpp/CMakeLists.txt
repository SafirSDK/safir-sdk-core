cmake_minimum_required(VERSION 2.6)
PROJECT (VehicleAppDb)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(VehicleAppDb ${sources})

TARGET_LINK_LIBRARIES(VehicleAppDb ${COMMON_LIBRARIES} ${ACE_LIBRARY})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(VehicleAppDb dose_cpp swre_interface_cpp olib)
endif()

INSTALL(TARGETS VehicleAppDb
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(VehicleAppDb)