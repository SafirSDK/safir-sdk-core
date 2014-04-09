find_package(CSharp REQUIRED)
INCLUDE(CSharpMacros)

SET(CS_FLAGS ${COMMON_CS_FLAGS} "-r:${SAFIR_RUNTIME}/bin/Safir.Time.dll" )

ADD_CS_EXECUTABLE(dotnet_tester test.cs ALL)

ADD_TEST(TimeProvider_dotnet_test ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(TEST TimeProvider_dotnet_test)
