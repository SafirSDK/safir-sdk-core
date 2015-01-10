FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(dose_test_cpp ${sources} ${headers})


TARGET_LINK_LIBRARIES(dose_test_cpp PRIVATE
  swre_interface_cpp
  dose_cpp
  safir_generated-DoseTest-cpp
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS dose_test_cpp TEST_SUITE)
