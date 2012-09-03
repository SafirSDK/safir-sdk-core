cmake_minimum_required(VERSION 2.8)

project(dose_com_control CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
FILE(GLOB dose_com_sources ${SAFIR_SDK}/include/Safir/Dob/Internal/DoseComAux/*.cpp)

ADD_EXECUTABLE(dose_com_control ${sources} ${dose_com_sources} ${headers})

TARGET_LINK_LIBRARIES(dose_com_control ${COMMON_LIBRARIES})

INSTALL(TARGETS dose_com_control
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dose_com_control)
