FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
SET(dose_com_sources ${safir_sdk_core_SOURCE_DIR}/src/dose/dose_communication.ss/src/dose_com/DoseOsSharedMem.cpp)

ADD_EXECUTABLE(dose_com_control ${sources} ${dose_com_sources} ${headers})

target_include_directories(dose_com_control PRIVATE ${safir_sdk_core_SOURCE_DIR}/src/dose/dose_communication.ss/src/defs/)
target_include_directories(dose_com_control PRIVATE ${safir_sdk_core_SOURCE_DIR}/src/dose/dose_communication.ss/src/dose_com/)

SAFIR_INSTALL(TARGETS dose_com_control TEST_SUITE)
