if (NOT CSHARP_FOUND)
  return()
endif()

FILE(GLOB sources *.cs)
FILE(GLOB resx_files *.resx)

ADD_CSHARP_ASSEMBLY(sate WINEXE
  SIGN ${safir_sdk_core_SOURCE_DIR}/../build/config/sdk/data/build/safirkey.snk
  SOURCES ${sources}
  RESOURCES ${resx_files}
  RESOURCE_PREFIX Sate.
  REFERENCES Safir.Dob dots_generated-Safir-dotnet Safir.Dob.Typesystem System.Windows.Forms System.Drawing System.Data)

INSTALL_CSHARP_ASSEMBLY(TARGET sate
  DESTINATION bin)
