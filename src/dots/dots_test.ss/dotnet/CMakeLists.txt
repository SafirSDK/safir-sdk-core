if (NOT CSHARP_FOUND)
  return()
endif()

ADD_CSHARP_ASSEMBLY(dots_test_dotnet EXE
  SOURCES AssemblyInfo.cs DotsTestDotnet.cs
  REFERENCES Safir.Dob.Typesystem safir_generated-DotsTest-dotnet)

#add dependencies to DotsTestExtra, but with a dummy target in between, to 
#avoid the dll becoming a direct reference in the C# project if built with
#MSVC IDE.
add_custom_target(safir_generated-DotsTestExtra-dotnet_dummy_dep DEPENDS safir_generated-DotsTestExtra-dotnet)
add_dependencies(dots_test_dotnet safir_generated-DotsTestExtra-dotnet_dummy_dep)

