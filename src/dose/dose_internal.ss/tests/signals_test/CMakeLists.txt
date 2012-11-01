ADD_EXECUTABLE(signals_test test.cpp ../../src/Signals.cpp)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(signals_test dots_generated-Safir-cpp dots_generated-cpp dots_cpp lluf_utils ${Boost_LIBRARIES})
endif()

ENABLE_TESTING()
ADD_TEST(Signals signals_test)

set_tests_properties(Signals PROPERTIES TIMEOUT 60)

