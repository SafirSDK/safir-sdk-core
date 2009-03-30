PROJECT(VehicleAppCs)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB cs_files *.cs)

ADD_CUSTOM_TARGET(
  VehicleAppCs.exe ALL 
  COMMAND gmcs -target:exe -r:Safir.Dob.Typesystem.dll,Safir.Dob.dll,dots_generated-dotnet.dll,swre_interface_dotnet.dll -out:VehicleAppCs.exe ${cs_files}
  DEPENDS ${cs_files}
  )

INSTALL(FILES VehicleAppCs.exe
        DESTINATION ${SAFIR_RUNTIME}/bin)

