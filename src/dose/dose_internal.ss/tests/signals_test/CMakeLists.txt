ADD_EXECUTABLE(signals_test test.cpp ../../src/Signals.cpp)

#disable lock checks since they mean we would have to 
add_definitions(-DDOSE_NO_LOCK_CHECKING)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(signals_test dots_generated-Safir-cpp dots_generated-cpp dots_cpp lluf_utils ${Boost_LIBRARIES})
endif()

ENABLE_TESTING()
ADD_TEST(Signals signals_test)

SET_SAFIR_TEST_PROPERTIES(Signals 60)
