if (NOT CSHARP_FOUND)
  return()
endif()

FILE(GLOB sources *.cs)

ADD_CSHARP_ASSEMBLY(dose_test_dotnet EXE 
  SOURCES ${sources}
  REFERENCES Safir.Dob Safir.SwReports safir_generated-DoseTest-dotnet Safir.Dob.Typesystem dots_generated-Safir-dotnet)

#TODO sign?
#TODO install

