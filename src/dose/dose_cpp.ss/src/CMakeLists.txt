FILE(GLOB_RECURSE headers *.h)
FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_cpp SHARED ${sources} ${headers})

target_include_directories(dose_cpp PUBLIC .)

TARGET_LINK_LIBRARIES(dose_cpp PRIVATE 
  dose_internal
  dose_dll
  dots_generated-Safir-cpp)

INSTALL(DIRECTORY Safir DESTINATION ${SAFIR_SDK}/include
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

INSTALL(TARGETS dose_cpp
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dose_cpp)
