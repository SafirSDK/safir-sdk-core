find_package(ACE)

if (ACE_FOUND)
  INCLUDE_DIRECTORIES(${ACE_INCLUDE_DIRECTORIES})

  FILE(GLOB headers *.h)
  FILE(GLOB sources *.cpp)
  
  ADD_EXECUTABLE(SplitJoinStresser ${sources} ${headers})
  
  #FIND_PACKAGE(Boost COMPONENTS program_options)
  
#  TARGET_LINK_LIBRARIES(SplitJoinStresser PRIVATE ${ACE_LIBRARY})
  
#  TARGET_LINK_LIBRARIES(SplitJoinStresser PRIVATE dose_cpp ${Boost_LIBRARIES})

  #TODO: install
  
else()
  MESSAGE(STATUS "ACE library was not found, not building SplitJoinStresser")

endif()

