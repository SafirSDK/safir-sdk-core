if (NOT CSHARP_FOUND)
  return()
endif()

FILE(GLOB sources *.cs)
FILE(GLOB resx_files *.resx)

ADD_CSHARP_ASSEMBLY(sate WINEXE
  SIGN ${safir-sdk-core_SOURCE_DIR}/src/cmake/safirkey.snk
  ICON sate_logo.ico
  SOURCES ${sources}
  RESOURCES PREFIX Sate. ${resx_files}
  REFERENCES Safir.Dob safir_generated-Core-dotnet Safir.Dob.Typesystem System.Windows.Forms System.Drawing System.Data)

SAFIR_INSTALL(TARGETS sate DEVELOPMENT)

