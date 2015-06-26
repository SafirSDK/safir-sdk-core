find_package(ACE)

if (ACE_FOUND)
  INCLUDE_DIRECTORIES(${ACE_INCLUDE_DIRECTORIES})

  ADD_EXECUTABLE(AceTimeConverter_test test.cpp test2.cpp)

  TARGET_LINK_LIBRARIES(AceTimeConverter_test PRIVATE
    douf_time_cpp
    ${ACE_LIBRARY}
	${Boost_DATE_TIME_LIBRARY})

  ADD_TEST(NAME AceTimeConverter_test COMMAND AceTimeConverter_test)

  SET_SAFIR_TEST_PROPERTIES(TEST AceTimeConverter_test)

else()
  MESSAGE(STATUS "ACE library was not found, not building and running the test for AceTimeConverter")
endif()
