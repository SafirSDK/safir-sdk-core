ADD_EXECUTABLE(signals_test test.cpp ../../src/Signals.cpp)

#disable lock checks since they mean we would have to 
add_definitions(-DDOSE_NO_LOCK_CHECKING)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(signals_test dots_generated-Safir-cpp dots_generated-cpp dots_cpp lluf_utils ${Boost_LIBRARIES})
endif()

ENABLE_TESTING()
ADD_TEST(Signals signals_test)

SET_TESTS_PROPERTIES(Signals PROPERTIES TIMEOUT 60)
SET_TESTS_PROPERTIES(Signals PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")
