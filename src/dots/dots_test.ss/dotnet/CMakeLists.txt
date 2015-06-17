if (NOT CSHARP_FOUND)
  return()
endif()

ADD_CSHARP_ASSEMBLY(dots_test_dotnet EXE
  SOURCES AssemblyInfo.cs DotsTestDotnet.cs DeepClone.cs
  REFERENCES Safir.Dob.Typesystem safir_generated-DotsTest-dotnet)

