if (NOT CSHARP_FOUND)
  return()
endif()

FILE(GLOB sources *.cs)

SET(CS_FLAGS ${COMMON_CS_FLAGS} "-r:${SAFIR_RUNTIME}/bin/Safir.Dob.Typesystem.dll,${SAFIR_RUNTIME}/bin/dots_generated-dotnet.dll" )

ADD_CS_EXECUTABLE(dots_test_dotnet "${sources}" ALL)
