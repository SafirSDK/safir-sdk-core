FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(RequestPingPong ${sources} ${headers})


TARGET_LINK_LIBRARIES(RequestPingPong PRIVATE
  dose_cpp
  safir_generated-DoseTest-cpp
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS RequestPingPong TEST_SUITE)
