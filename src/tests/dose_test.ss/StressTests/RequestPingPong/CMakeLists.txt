FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(RequestPingPong ${sources} ${headers})


TARGET_LINK_LIBRARIES(RequestPingPong PRIVATE
  douf_time_cpp
  dose_cpp 
  dose_dll
  dose_internal
  dots_generated-DoseTest-cpp 
  dots_generated-Safir-cpp 
  dots_cpp
  lluf_utils 
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})


#TODO: install
