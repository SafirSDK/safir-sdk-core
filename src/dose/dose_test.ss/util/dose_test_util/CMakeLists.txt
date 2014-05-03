#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
SET(dose_com_sources ../../../dose_communication.ss/src/dose_com/DoseOsSharedMem.cpp)

ADD_LIBRARY(dose_test_util SHARED ${sources} ${dose_com_sources} ${headers})

TARGET_INCLUDE_DIRECTORIES(dose_test_util PUBLIC .)

target_include_directories(dose_test_util PRIVATE ../../../dose_communication.ss/src/defs/)
target_include_directories(dose_test_util PRIVATE ../../../dose_communication.ss/src/dose_com/)

target_link_libraries(dose_test_util PRIVATE
  dots_cpp
  dots_internal
  lluf_utils)

INSTALL(DIRECTORY Safir 
  DESTINATION ${SAFIR_SDK}/include
  PATTERN ".svn" EXCLUDE
  PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dose_test_util
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL(FILES dose_test_util.gpr
        DESTINATION ${SAFIR_SDK}/ada)

INSTALL_DEBUG_INFO(dose_test_util)
