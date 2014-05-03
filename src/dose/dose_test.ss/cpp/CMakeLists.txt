FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
SET(dose_com_sources ../../dose_communication.ss/src/dose_com/DoseOsSharedMem.cpp)

ADD_EXECUTABLE(dose_test_cpp ${sources} ${dose_com_sources} ${headers})

target_include_directories(dose_test_cpp PRIVATE ../../dose_communication.ss/src/defs/)
target_include_directories(dose_test_cpp PRIVATE ../../dose_communication.ss/src/dose_com/)

TARGET_LINK_LIBRARIES(dose_test_cpp PRIVATE 
  swre_interface_cpp 
  dose_cpp 
  dose_internal
  dots_generated-Safir-cpp
  dots_generated-DoseTest-cpp 
  dots_cpp
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})


INSTALL(TARGETS dose_test_cpp
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dose_test_cpp)
