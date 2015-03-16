#We allow the installation of the python test support stuff since the dose and dope tests
#need them to be installed (they are run without access to source code).
#All other uses of these classes should access them from the source tree, using
#SET_SAFIR_TEST_PROPERTIES

INSTALL(FILES
  python/syslog_server.py
  python/testenv.py
  DESTINATION ${SAFIR_INSTALL_DESTINATION_TEST_DATA}/python
  COMPONENT Test)
