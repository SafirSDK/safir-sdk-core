find_package(ACE)

if (ACE_FOUND)
  INCLUDE_DIRECTORIES(${ACE_INCLUDE_DIRECTORIES})

  ADD_EXECUTABLE(AceTimeConverter_test test.cpp)
  
  if (NOT MSVC)
    TARGET_LINK_LIBRARIES(AceTimeConverter_test douf_time_cpp)
  endif()
  
  TARGET_LINK_LIBRARIES(AceTimeConverter_test ${ACE_LIBRARY})
  
  ADD_TEST(AceTimeConverter_test AceTimeConverter_test)
else()
  MESSAGE(STATUS "ACE library was not found, not building and running the test for AceTimeConverter")
endif()
