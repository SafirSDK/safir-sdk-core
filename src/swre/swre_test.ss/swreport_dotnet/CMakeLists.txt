find_package(CSharp REQUIRED)
INCLUDE(CSharpMacros)

SET(CS_FLAGS ${COMMON_CS_FLAGS} "-r:${SAFIR_RUNTIME}/bin/Safir.SwReports.dll" )

ADD_CS_EXECUTABLE(swreport_sender_dotnet sender.cs ALL)

ADD_TEST(swreport_dotnet ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(TEST swreport_dotnet TIMEOUT 120)
