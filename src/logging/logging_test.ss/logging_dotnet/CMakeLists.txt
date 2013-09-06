find_package(CSharp REQUIRED)
INCLUDE(CSharpMacros)

SET(CS_FLAGS ${COMMON_CS_FLAGS} "-r:${SAFIR_RUNTIME}/bin/Safir.Logging.dll" )

ADD_CS_EXECUTABLE(log_sender_dotnet sender.cs ALL)

ADD_TEST(logging_dotnet ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_TESTS_PROPERTIES(logging_dotnet PROPERTIES TIMEOUT 120)
SET_PROPERTY(TEST logging_dotnet
  PROPERTY ENVIRONMENT 
  "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config"
  ##"PYTHONPATH=$ENV{PYTHONPATH}:${SOURCE_ROOT}/src/tests/test_support/python")
  "PYTHONPATH=${SOURCE_ROOT}/src/tests/test_support/python")

