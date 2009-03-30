PROJECT(VehicleMmiCsWinForms)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB cs_files *.cs)
FILE(GLOB resx_files *.resx)

FILE(GLOB property_files Properties/*.cs)
FILE(GLOB property_resx_files Properties/*.resx)

FOREACH(resx_file ${resx_files})
  STRING(REGEX REPLACE "/([a-zA-Z.0-9]+).resx" "/VehicleMmiCsWinForms.\\1.resources" resource_file ${resx_file})
  SET(resource_files ${resource_files} ${resource_file})
  SET(resource_command_line ${resource_command_line} -res:${resource_file})

  ADD_CUSTOM_COMMAND(OUTPUT ${resource_file}
                     COMMAND resgen2 ARGS ${resx_file} ${resource_file}
                     DEPENDS ${resx_file})
ENDFOREACH(resx_file)

FOREACH(resx_file ${property_resx_files})
  STRING(REGEX REPLACE "/([a-zA-Z.0-9]+).resx" "/VehicleMmiCsWinForms.Properties.\\1.resources" resource_file ${resx_file})
  SET(property_resource_files ${property_resource_files} ${resource_file})
  SET(resource_command_line ${resource_command_line} -res:${resource_file})

  ADD_CUSTOM_COMMAND(OUTPUT ${resource_file}
                     COMMAND resgen2 ARGS ${resx_file} ${resource_file}
                     WORKING_DIRECTORY Properties
                     DEPENDS ${resx_file})
ENDFOREACH(resx_file)


ADD_CUSTOM_TARGET(
  VehicleMmiCsWinForms.exe ALL 
  COMMAND gmcs -target:exe -r:Safir.Dob.Typesystem.dll,Safir.Dob.dll,dots_generated-dotnet.dll,swre_interface_dotnet.dll,System.Windows.Forms.dll,System.Drawing.dll -out:VehicleMmiCsWinForms.exe ${cs_files} ${property_files} ${resource_command_line}
  DEPENDS ${cs_files} ${property_files} ${resource_files} ${property_resource_files}
  )

INSTALL(FILES VehicleMmiCsWinForms.exe
        DESTINATION ${SAFIR_RUNTIME}/bin)

