if (NOT CSHARP_FOUND)
  return()
endif()

ADD_CSHARP_ASSEMBLY(Safir.SwReports LIBRARY
  SIGN ${safir-sdk-core_SOURCE_DIR}/src/cmake/safirkey.snk
  SOURCES Library.cs backdoor.cs sw_reports.cs CrashReporter.cs Tracer.cs AssemblyInfo.cs
  REFERENCES Safir.Dob safir_generated-Core-dotnet Safir.Dob.Typesystem Safir.Logging
  DLL_IMPORTS swre_library)

SAFIR_INSTALL(TARGETS Safir.SwReports)
