find_package(ACE)

if (ACE_FOUND)
  INCLUDE_DIRECTORIES(${ACE_INCLUDE_DIRECTORIES})

  FILE(GLOB headers *.h)
  FILE(GLOB sources *.cpp)

  ADD_EXECUTABLE(SplitJoinTest ${sources} ${headers})

  TARGET_LINK_LIBRARIES(SplitJoinTest PRIVATE
    dose_cpp
    safir_generated-DoseTest-cpp
    ${Boost_PROGRAM_OPTIONS_LIBRARY}
    ${ACE_LIBRARY})

  SAFIR_INSTALL(TARGETS SplitJoinTest TEST_SUITE)

else()
  MESSAGE(STATUS "ACE library was not found, not building SplitJoinTest")

endif()
