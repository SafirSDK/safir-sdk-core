PROJECT(swre_interface_dotnet)

FILE(GLOB cs_files *.cs)

ADD_CUSTOM_COMMAND(OUTPUT swre_interface_dotnet.dll
                  COMMAND gmcs -target:library -r:Safir.Dob.Typesystem.dll,Safir.Dob.dll,dots_generated-dotnet.dll -out:swre_interface_dotnet.dll ${cs_files}
				  DEPENDS ${cs_files}
                  COMMENT "Building swre_interface_dotnet.dll"
                  VERBATIM)

ADD_CUSTOM_TARGET(swre_interface_dotnet ALL
				  DEPENDS swre_interface_dotnet.dll)

INSTALL(FILES swre_interface_dotnet.dll
		DESTINATION $ENV{SAFIR_RUNTIME}/lib)

