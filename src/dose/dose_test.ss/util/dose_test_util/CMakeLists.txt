project(dose_test_util CXX)

#this library has a C interface, so we don't want the "d" suffix on debug builds on windows
# NO_LIBRARY_POSTFIXES 1 is set in the top level CMakeLists.txt file

INCLUDE_DIRECTORIES(.)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
FILE(GLOB dose_com_sources ${SAFIR_SDK}/include/Safir/Dob/Internal/DoseComAux/*.cpp)

ADD_DEFINITIONS(-DDOSE_EXPORTS)

ADD_LIBRARY(dose_test_util SHARED ${sources} ${dose_com_sources} ${headers})

TARGET_LINK_LIBRARIES(dose_test_util ${COMMON_LIBRARIES})

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
