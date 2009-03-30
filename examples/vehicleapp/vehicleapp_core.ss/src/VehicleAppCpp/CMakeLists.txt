PROJECT (VehicleAppCpp)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(VehicleAppCpp ${sources})

TARGET_LINK_LIBRARIES(VehicleAppCpp dose_cpp swre_interface_cpp)

INSTALL(TARGETS VehicleAppCpp
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
