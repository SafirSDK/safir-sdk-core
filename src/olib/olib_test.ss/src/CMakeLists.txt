add_executable(olib_test main.cpp DbOlibTest.cpp DbOlibTest.h)

TARGET_LINK_LIBRARIES(olib_test PRIVATE
  olib
  safir_generated-OlibTest-cpp
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

SAFIR_INSTALL(TARGETS olib_test TEST_SUITE)

SAFIR_INSTALL_PYTHON(SCRIPT run_olib_tests.py
  COMPONENT Test)
