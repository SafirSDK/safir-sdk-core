ADD_EXECUTABLE(sate_tester data_generator.cpp)

TARGET_LINK_LIBRARIES(sate_tester PRIVATE
  lluf_internal
  dose_cpp
  safir_generated-DoseTest-cpp)

SAFIR_INSTALL(TARGETS sate_tester COMPONENT TestSuite)
