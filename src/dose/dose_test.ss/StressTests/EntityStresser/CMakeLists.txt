FILE(GLOB sources *.cpp ../common/*.cpp)
FILE(GLOB headers *.h ../common/*.h)

ADD_EXECUTABLE(EntityStresser ${sources} ${headers})

TARGET_LINK_LIBRARIES(EntityStresser PRIVATE 
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

