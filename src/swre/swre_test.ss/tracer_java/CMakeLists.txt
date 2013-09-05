find_package(Java REQUIRED)

INCLUDE(JavaMacros)

SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/swre_application_java.jar)

SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(swre_test_java Sender.java ALL)

ADD_TEST(tracer_java ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_TESTS_PROPERTIES(tracer_java PROPERTIES TIMEOUT 120)
SET_PROPERTY(TEST tracer_java 
  PROPERTY ENVIRONMENT 
  "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config"
  "PYTHONPATH=$ENV{PYTHONPATH}:${SOURCE_ROOT}/src/tests/test_support/python")
