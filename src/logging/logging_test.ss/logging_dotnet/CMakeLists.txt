if (NOT CSHARP_FOUND)
  return()
endif()

ADD_CSHARP_ASSEMBLY(log_sender_dotnet EXE SOURCES sender.cs REFERENCES Safir.Logging)

ADD_TEST(NAME logging_dotnet
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --safir-show-config $<TARGET_FILE:safir_show_config>
  --sender-exe $<TARGET_PROPERTY:log_sender_dotnet,ASSEMBLY_FILE>
  --dependencies $<TARGET_PROPERTY:Safir.Logging,ASSEMBLY_FILE>)

SET_SAFIR_TEST_PROPERTIES(TEST logging_dotnet TIMEOUT 120)

