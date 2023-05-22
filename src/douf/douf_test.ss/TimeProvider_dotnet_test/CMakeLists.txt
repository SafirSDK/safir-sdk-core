if (NOT CSHARP_FOUND)
  return()
endif()

ADD_CSHARP_ASSEMBLY(douf_time_test_dotnet EXE
  SOURCES test.cs
  REFERENCES Safir.Time)


ADD_TEST(NAME TimeProvider_dotnet_test 
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --test-exe  $<TARGET_GENEX_EVAL:douf_time_test_dotnet,$<TARGET_PROPERTY:douf_time_test_dotnet,ASSEMBLY_FILE>>)

SET_SAFIR_TEST_PROPERTIES(TEST TimeProvider_dotnet_test)
