#We allow the installation of the python test support stuff since the dose tests need them to be installed
#(dose tests are run without access to source code).
#All other uses of these classes should access them from the source tree, using SET_SAFIR_TEST_PROPERTIES

INSTALL(FILES
  python/syslog_server.py
  DESTINATION ${SAFIR_INSTALL_DESTINATION_TEST_DATA}/python
  COMPONENT Test)
