if (NOT CSHARP_FOUND)
  return()
endif()

ADD_CSHARP_ASSEMBLY(douf_time_test_dotnet EXE
  SOURCES test.cs
  REFERENCES Safir.Time)


ADD_TEST(NAME TimeProvider_dotnet_test 
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --test-exe $<TARGET_PROPERTY:douf_time_test_dotnet,ASSEMBLY_FILE>
  --dependencies $<TARGET_PROPERTY:Safir.Dob.Typesystem,ASSEMBLY_FILE>,$<TARGET_PROPERTY:Safir.Time,ASSEMBLY_FILE>)

SET_SAFIR_TEST_PROPERTIES(TEST TimeProvider_dotnet_test
                          CONFIG_OVERRIDE "${CMAKE_CURRENT_SOURCE_DIR}/../config")
