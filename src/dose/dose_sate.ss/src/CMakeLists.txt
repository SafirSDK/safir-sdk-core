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

#TODO install



#SET(CS_FLAGS ${COMMON_CS_FLAGS} "-r:${SAFIR_RUNTIME}/bin/Safir.Dob.Typesystem.dll,${SAFIR_RUNTIME}/bin/dots_generated-dotnet.dll,${SAFIR_RUNTIME}/bin/Safir.Dob.dll,System.Windows.Forms.dll,System.Drawing.dll,System.Data.dll" )

#ADD_CS_RESOURCES("${resx_files}" Sate .)
#ADD_CS_GUI_EXECUTABLE(sate "${sources}" ALL)

# #The csharp build renames the exe to sate.csexe, so we rename it back when we install
# INSTALL(PROGRAMS ${target_EXE}
#   DESTINATION ${SAFIR_RUNTIME}/bin
#   RENAME sate.exe)

# INSTALL_CS_DEBUG_INFO()
