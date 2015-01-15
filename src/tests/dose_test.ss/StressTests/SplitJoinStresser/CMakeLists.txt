find_package(ACE)

if (ACE_FOUND)
  INCLUDE_DIRECTORIES(${ACE_INCLUDE_DIRECTORIES})

  FILE(GLOB headers *.h)
  FILE(GLOB sources *.cpp)

  ADD_EXECUTABLE(SplitJoinStresser ${sources} ${headers})

  TARGET_LINK_LIBRARIES_FOOBAR(SplitJoinStresser PRIVATE
    dose_cpp
    safir_generated-DoseTest-cpp
    ${Boost_PROGRAM_OPTIONS_LIBRARY}
    ${ACE_LIBRARY})

  SAFIR_INSTALL(TARGETS SplitJoinStresser TEST_SUITE)

else()
  MESSAGE(STATUS "ACE library was not found, not building SplitJoinStresser")

endif()
