#The test suite for a deprecated feature obviously doesn't want deprecation warnings...
add_definitions(-DSAFIR_DISABLE_OLIB_DEPRECATION_WARNING)

add_executable(olib_test main.cpp DbOlibTest.cpp DbOlibTest.h)

TARGET_LINK_LIBRARIES(olib_test PRIVATE
  olib
  safir_generated-OlibTest-cpp
  ${Boost_PROGRAM_OPTIONS_LIBRARY})

#Olib tests are not installed since the run_olib_tests.py is not updated.
#since Olib is deprecated this is low priority work.

#SAFIR_INSTALL(TARGETS olib_test TEST_SUITE)

#SAFIR_INSTALL_PYTHON(SCRIPT run_olib_tests.py
#  COMPONENT TestSuite)
