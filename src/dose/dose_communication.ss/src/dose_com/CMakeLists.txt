PROJECT (dose_com)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

SET(CMAKE_CXX_FLAGS "-D_LINUX")

FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_com SHARED ${sources})

INSTALL(FILES ../defs/DoseCom_Interface.h 
              ../defs/DoseCom_Interface_Classes.h
        DESTINATION $ENV{SAFIR_SDK}/include/Safir/Dob/Internal/)

INSTALL(FILES DosePlatform.h
              PrintError.h
              DoseOsInterface.h
              DoseOsSharedMem.cpp
              ../defs/DoseNodeStatus.h
        DESTINATION $ENV{SAFIR_SDK}/include/Safir/Dob/Internal/DoseComAux/)


INSTALL(TARGETS dose_com
        RUNTIME DESTINATION $ENV{SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION $ENV{SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION $ENV{SAFIR_SDK}/lib)
