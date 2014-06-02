if (NOT CSHARP_FOUND)
  return()
endif()

ADD_CSHARP_ASSEMBLY(swreport_sender_dotnet EXE 
  SOURCES sender.cs 
  REFERENCES Safir.SwReports)

ADD_TEST(NAME swreport_dotnet
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --safir-show-config $<TARGET_FILE:safir_show_config>
  --sender-exe $<TARGET_PROPERTY:swreport_sender_dotnet,ASSEMBLY_FILE>
  --dependencies $<TARGET_PROPERTY:Safir.SwReports,ASSEMBLY_FILE>,$<TARGET_PROPERTY:Safir.Dob.Typesystem,ASSEMBLY_FILE>)

SET_SAFIR_TEST_PROPERTIES(TEST swreport_dotnet TIMEOUT 120)
