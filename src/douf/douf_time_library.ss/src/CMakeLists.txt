#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

FILE(GLOB sources *.cpp)

ADD_LIBRARY(douf_time_library SHARED ${sources})

target_include_directories(douf_time_library PUBLIC .)

FIND_PACKAGE(Boost COMPONENTS regex thread system)
    
TARGET_LINK_LIBRARIES(douf_time_library 
  dots_generated-douf_time-cpp 
  dots_cpp 
  ${Boost_LIBRARIES})

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS douf_time_library
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL(FILES ../data/douf_time_library.gpr
        DESTINATION ${SAFIR_SDK}/ada)

INSTALL_DEBUG_INFO(douf_time_library)
