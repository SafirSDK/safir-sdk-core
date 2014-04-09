find_package(CSharp REQUIRED)
INCLUDE(CSharpMacros)

#SET(CS_FLAGS ${COMMON_CS_FLAGS} "-r:${SAFIR_RUNTIME}/bin/Safir.Dob.Typesystem.dll,${SAFIR_RUNTIME}/bin/dots_generated-dotnet.dll" )
SET(CS_FLAGS ${COMMON_CS_FLAGS} "-r:${SAFIR_RUNTIME}/bin/Safir.SwReports.dll" )

ADD_CS_EXECUTABLE(tracer_sender_dotnet sender.cs ALL)

ADD_TEST(tracer_dotnet ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(TEST tracer_dotnet TIMEOUT 120)
