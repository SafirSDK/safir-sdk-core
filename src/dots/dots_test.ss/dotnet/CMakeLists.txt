if (NOT CSHARP_FOUND)
  return()
endif()

#SET(CS_FLAGS ${COMMON_CS_FLAGS} "-r:${SAFIR_RUNTIME}/bin/Safir.Dob.Typesystem.dll,${SAFIR_RUNTIME}/bin/dots_generated-dotnet.dll" )

ADD_CSHARP_ASSEMBLY(dots_test_dotnet EXE 
  SOURCES AssemblyInfo.cs DotsTestDotnet.cs
  REFERENCES Safir.Dob.Typesystem dots_generated-DotsTest-dotnet)

