FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)
SET(dose_com_sources ${safir_sdk_core_SOURCE_DIR}/src/dose/dose_communication.ss/src/dose_com/DoseOsSharedMem.cpp)

ADD_EXECUTABLE(dose_test_cpp ${sources} ${dose_com_sources} ${headers})

target_include_directories(dose_test_cpp PRIVATE ${safir_sdk_core_SOURCE_DIR}/src/dose/dose_communication.ss/src/defs/)
target_include_directories(dose_test_cpp PRIVATE ${safir_sdk_core_SOURCE_DIR}/src/dose/dose_communication.ss/src/dose_com/)

TARGET_LINK_LIBRARIES(dose_test_cpp PRIVATE 
  swre_interface_cpp 
  dose_cpp 
  safir_generated-DoseTest-cpp 
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})
  
SAFIR_INSTALL(TARGETS dose_test_cpp TEST_SUITE)
