if (NOT CSHARP_FOUND)
  return()
endif()

SET(CS_FLAGS ${COMMON_CS_FLAGS} "-r:${SAFIR_RUNTIME}/bin/Safir.Logging.dll" )

ADD_CS_EXECUTABLE(log_sender_dotnet sender.cs ALL)

ADD_TEST(logging_dotnet ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(TEST logging_dotnet TIMEOUT 120)

