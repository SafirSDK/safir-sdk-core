if (NOT CSHARP_FOUND)
  return()
endif()

FILE(GLOB sources *.cs)

ADD_CSHARP_ASSEMBLY(dose_test_dotnet EXE
  SOURCES ${sources}
  REFERENCES Safir.Dob Safir.SwReports safir_generated-DoseTest-dotnet Safir.Dob.Typesystem safir_generated-Core-dotnet
  DLL_IMPORTS dose_test_util)

SAFIR_INSTALL(TARGETS dose_test_dotnet TEST_SUITE)

