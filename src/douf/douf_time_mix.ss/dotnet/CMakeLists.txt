PROJECT(douf_time_dotnet)

cmake_minimum_required(VERSION 2.6)

FILE(GLOB cs_files *.cs)

ADD_CUSTOM_COMMAND(OUTPUT douf_time_dotnet.dll
                  COMMAND gmcs -target:library -r:Safir.Dob.Typesystem.dll,Safir.Dob.dll,dots_generated-dotnet.dll -out:douf_time_dotnet.dll ${cs_files}
				  DEPENDS ${cs_files}
                  COMMENT "Building douf_time_dotnet.dll"
                  VERBATIM)

ADD_CUSTOM_TARGET(douf_time_dotnet ALL
				  DEPENDS douf_time_dotnet.dll)

INSTALL(FILES douf_time_dotnet.dll
		DESTINATION $ENV{SAFIR_RUNTIME}/lib)

