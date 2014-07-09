if (NOT CSHARP_FOUND)
  return()
endif()

ADD_CSHARP_ASSEMBLY(Safir.SwReports LIBRARY 
  SIGN ${safir_sdk_core_SOURCE_DIR}/build/config/sdk/data/build/safirkey.snk
  SOURCES Library.cs backdoor.cs sw_reports.cs CrashReporter.cs Tracer.cs AssemblyInfo.cs
  REFERENCES Safir.Dob safir_generated-Core-dotnet Safir.Dob.Typesystem Safir.Logging)

add_dependencies(Safir.SwReports swre_library)

SAFIR_INSTALL(TARGETS Safir.SwReports)
