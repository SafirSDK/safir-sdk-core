cmake_minimum_required(VERSION 2.8)

project(dose_com CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB headers *.h ../defs/*.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_com SHARED ${sources} ${headers})

ADD_DEFINITIONS(-DDOSE_COM_EXPORTS)

if (WIN32)
    TARGET_LINK_LIBRARIES(dose_com ws2_32)
endif()

INSTALL(FILES ../defs/DoseCom_Interface.h 
              ../defs/DoseCom_Interface_Classes.h
        DESTINATION ${SAFIR_SDK}/include/Safir/Dob/Internal/)

INSTALL(FILES DosePlatform.h
              PrintError.h
              DoseOsInterface.h
              DoseOsSharedMem.cpp
              ../defs/DoseNodeStatus.h
        DESTINATION ${SAFIR_SDK}/include/Safir/Dob/Internal/DoseComAux/)


INSTALL(TARGETS dose_com
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)


INSTALL_DEBUG_INFO(dose_com)
