
FILE(GLOB sources *.cs Properties/*.cs)
FILE(GLOB resx_files *.resx)
FILE(GLOB property_resx_files Properties/*.resx)

ADD_CSHARP_ASSEMBLY(VehicleMmiCsWinForms WINEXE
  SOURCES ${sources}
  RESOURCES PREFIX VehicleMmiCsWinForms. ${resx_files}
            PREFIX VehicleMmiCsWinForms.Properties WORKING_DIRECTORY Properties ${property_resx_files}
  REFERENCES Safir.Dob safir_generated-Core-dotnet safir_generated-Example-dotnet Safir.Dob.Typesystem
             Safir.Logging Safir.SwReports System.Drawing System.Windows.Forms
  NOVERSION)

#TODO: install?
