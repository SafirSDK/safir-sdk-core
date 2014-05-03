FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
SET(dose_com_sources ../../../dose_communication.ss/src/dose_com/DoseOsSharedMem.cpp)

ADD_EXECUTABLE(dose_com_control ${sources} ${dose_com_sources} ${headers})

target_include_directories(dose_com_control PRIVATE ../../../dose_communication.ss/src/defs/)
target_include_directories(dose_com_control PRIVATE ../../../dose_communication.ss/src/dose_com/)

#TODO: hmmm, dose_com_control doesnt really need to link against dose_com, I added that to get the PUBLIC includes. Better way?


# INSTALL(TARGETS dose_com_control
#         RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
#         LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
#         ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

# INSTALL_DEBUG_INFO(dose_com_control)
