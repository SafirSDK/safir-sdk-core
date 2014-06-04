FILE(GLOB sources *.cpp ../common/*.cpp)
FILE(GLOB headers *.h ../common/*.h)

ADD_EXECUTABLE(SignalTest ${sources} ${headers})

TARGET_LINK_LIBRARIES(SignalTest PRIVATE
  dose_internal
  dots_cpp
  dots_kernel
  dots_internal
  lluf_utils 
  ${Boost_PROGRAM_OPTIONS_LIBRARY}
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})


#TODO: install
