FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(ss_test ${sources})

IF (NOT MSVC)
  TARGET_LINK_LIBRARIES(ss_test lluf_startup_synchronizer ${Boost_LIBRARIES})
ENDIF()

ADD_TEST(StartupSynchronizer_processes ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)
SET_TESTS_PROPERTIES(StartupSynchronizer_processes PROPERTIES TIMEOUT 120)
SET_TESTS_PROPERTIES(StartupSynchronizer_processes PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")
