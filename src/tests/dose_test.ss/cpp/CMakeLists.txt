FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_test_cpp ${sources} ${headers})


TARGET_LINK_LIBRARIES(dose_test_cpp PRIVATE
  swre_interface_cpp
  dose_cpp
  lluf_internal
  safir_generated-DoseTest-cpp
  Boost::thread)

SAFIR_INSTALL(TARGETS dose_test_cpp COMPONENT TestSuite)
