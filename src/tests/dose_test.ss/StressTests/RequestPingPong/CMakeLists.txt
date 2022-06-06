FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(RequestPingPong ${sources} ${headers})


TARGET_LINK_LIBRARIES(RequestPingPong PRIVATE
  dose_cpp
  safir_generated-DoseTest-cpp
  lluf_internal
  Boost::program_options)

SAFIR_INSTALL(TARGETS RequestPingPong COMPONENT TestSuite)
