PROJECT(dose_test_dotnet)

FILE(GLOB cs_files *.cs)

ADD_CUSTOM_COMMAND(OUTPUT dose_test_dotnet.exe
                  COMMAND gmcs -r:Safir.Dob.Typesystem.dll,Safir.Dob.dll,dots_generated-dotnet.dll -out:dose_test_dotnet.exe ${cs_files}
				  DEPENDS ${cs_files}
                  COMMENT "Building dose_test_dotnet.exe"
                  VERBATIM)

ADD_CUSTOM_TARGET(dose_test_dotnet.exe ALL
				  DEPENDS dose_test_dotnet.exe)

INSTALL(PROGRAMS dose_test_dotnet.exe
		DESTINATION $ENV{SAFIR_RUNTIME}/bin)

