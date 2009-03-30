PROJECT (VehicleAppDb)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(VehicleAppDb ${sources})

TARGET_LINK_LIBRARIES(VehicleAppDb dose_cpp swre_interface_cpp olib)

INSTALL(TARGETS VehicleAppDb
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)
