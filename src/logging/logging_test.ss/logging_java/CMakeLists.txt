find_package(Java REQUIRED)

INCLUDE(JavaMacros)

file(GLOB_RECURSE sources *.java)

SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/logging_java.jar)

SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(log_sender_java "${sources}" ALL)

ADD_TEST(logging_java ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_TESTS_PROPERTIES(logging_java PROPERTIES TIMEOUT 120)
SET_PROPERTY(TEST logging_java
  PROPERTY ENVIRONMENT 
  "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config"
  "PYTHONPATH=$ENV{PYTHONPATH}:${SOURCE_ROOT}/src/tests/test_support/python")
