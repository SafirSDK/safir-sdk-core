add_executable(olib_test main.cpp DbOlibTest.cpp DbOlibTest.h)

TARGET_LINK_LIBRARIES(olib_test PRIVATE
  olib 
  safir_generated-OlibTest-cpp 
  safir_generated-Core-cpp 
  ${SAFIR_ODBC_LIBRARIES}
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

SAFIR_INSTALL(TARGETS olib_test TEST_SUITE)

INSTALL(PROGRAMS run_olib_tests.py
  DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN}
  COMPONENT Test)
