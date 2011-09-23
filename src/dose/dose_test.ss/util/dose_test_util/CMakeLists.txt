cmake_minimum_required(VERSION 2.4)

PROJECT (dose_test_util)

#this library has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including cmake_settings.txt
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
FILE(GLOB dose_com_sources ${SAFIR_SDK}/include/Safir/Dob/Internal/DoseComAux/*.cpp)

ADD_DEFINITIONS(-DDOSE_EXPORTS)

ADD_LIBRARY(dose_test_util SHARED ${sources} ${dose_com_sources} ${headers})

TARGET_LINK_LIBRARIES(dose_test_util ${COMMON_LIBRARIES} ${ACE_LIBRARY})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dose_test_util)
endif()


INSTALL(FILES dose_test_util.h              
        DESTINATION ${SAFIR_SDK}/include/Safir/Dob/Internal/DoseTest/)


INSTALL(TARGETS dose_test_util
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL(FILES dose_test_util.gpr
        DESTINATION ${SAFIR_SDK}/ada)

INSTALL_DEBUG_INFO(dose_test_util)