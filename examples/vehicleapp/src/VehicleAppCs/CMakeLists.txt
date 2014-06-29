if (NOT CSHARP_FOUND)
  return()
endif()


FILE(GLOB sources *.cs)

ADD_CSHARP_ASSEMBLY(VehicleAppCs EXE
  SOURCES ${sources}
  REFERENCES Safir.Dob safir_generated-Example-dotnet safir_generated-Core-dotnet Safir.Dob.Typesystem Safir.Logging Safir.SwReports)

#todo install?
