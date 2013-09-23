#We allow the installation of the python test support stuff since the dose tests need them to be installed
#(dose tests are run without access to source code).
#All other uses of these classes should access them from the source tree, using SET_SAFIR_TEST_PROPERTIES

cmake_minimum_required(VERSION 2.8)

project(test_support NONE)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

INSTALL(FILES
  python/syslog_server.py
  DESTINATION ${SAFIR_RUNTIME}/data/test_support/python)
