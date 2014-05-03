FILE(GLOB headers *.h ../defs/*.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_com SHARED ${sources} ${headers})

if (WIN32)
    TARGET_LINK_LIBRARIES(dose_com PRIVATE ws2_32)
endif()

TARGET_LINK_LIBRARIES(dose_com PRIVATE lluf_utils)

target_include_directories(dose_com PUBLIC ../defs)
target_include_directories(dose_com PUBLIC .)
#TODO: definitely move public stuff into better directories!

# INSTALL(FILES ../defs/DoseCom_Interface.h 
#               ../defs/DoseCom_Interface_Classes.h
#         DESTINATION ${SAFIR_SDK}/include/Safir/Dob/Internal/)

# INSTALL(FILES DosePlatform.h
#               PrintError.h
#               DoseOsInterface.h
#               DoseOsSharedMem.cpp
#               ../defs/DoseNodeStatus.h
#         DESTINATION ${SAFIR_SDK}/include/Safir/Dob/Internal/DoseComAux/)


# INSTALL(TARGETS dose_com
#         RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
#         LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
#         ARCHIVE DESTINATION ${SAFIR_SDK}/lib)


# INSTALL_DEBUG_INFO(dose_com)
