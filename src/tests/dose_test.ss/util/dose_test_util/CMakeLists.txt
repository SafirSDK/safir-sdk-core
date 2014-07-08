#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
SET(dose_com_sources ${safir_sdk_core_SOURCE_DIR}/src/dose/dose_communication.ss/src/dose_com/DoseOsSharedMem.cpp)

ADD_LIBRARY(dose_test_util SHARED ${sources} ${dose_com_sources} ${headers})

#Make our INTERNAL headers available to other parts of Core.
TARGET_INCLUDE_DIRECTORIES(dose_test_util PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

target_include_directories(dose_test_util PRIVATE ${safir_sdk_core_SOURCE_DIR}/src/dose/dose_communication.ss/src/defs/)
target_include_directories(dose_test_util PRIVATE ${safir_sdk_core_SOURCE_DIR}/src/dose/dose_communication.ss/src/dose_com/)

#target_link_libraries(dose_test_util PRIVATE
#  dots_cpp
#  dots_internal
#  lluf_utils)

SAFIR_INSTALL(TARGETS dose_test_util TEST_SUITE)

# INSTALL(FILES dose_test_util.gpr
#         DESTINATION ${SAFIR_SDK}/ada)
