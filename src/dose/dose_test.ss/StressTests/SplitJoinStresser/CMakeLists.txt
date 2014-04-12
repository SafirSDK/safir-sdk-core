cmake_minimum_required(VERSION 2.8)
project(SplitJoinStresser CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

find_package(ACE)

if (ACE_FOUND)
  INCLUDE_DIRECTORIES(${ACE_INCLUDE_DIRECTORIES})

  FILE(GLOB headers *.h)
  FILE(GLOB sources *.cpp)
  
  ADD_EXECUTABLE(SplitJoinStresser ${sources} ${headers})
  
  #FIND_PACKAGE(Boost COMPONENTS program_options)
  
  TARGET_LINK_LIBRARIES(SplitJoinStresser PRIVATE ${ACE_LIBRARY})
  
  if (NOT MSVC)
    TARGET_LINK_LIBRARIES(SplitJoinStresser PRIVATE dose_cpp ${Boost_LIBRARIES})
  endif()
  
  INSTALL(TARGETS SplitJoinStresser
          RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
          LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
          ARCHIVE DESTINATION ${SAFIR_SDK}/lib)
  
  INSTALL_DEBUG_INFO(SplitJoinStresser)
else()
  MESSAGE(STATUS "ACE library was not found, not building SplitJoinStresser")

  #dummy install to get an install target in the makefiles
  INSTALL (FILES
    DESTINATION dummy)

endif()

