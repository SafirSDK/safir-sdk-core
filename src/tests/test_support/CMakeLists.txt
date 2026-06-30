#We allow the installation of the python test support stuff since the dose and dope tests
#need them to be installed (they are run without access to source code).
#All other uses of these classes should access them from the source tree, using
#SET_SAFIR_TEST_PROPERTIES

SAFIR_INSTALL_PYTHON_MODULE(
  FILES
  python/syslog_server.py
  python/testenv.py
  python/output.py
  python/communication_test_support.py
  COMPONENT TestSuite)
