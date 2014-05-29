if (NOT Java_FOUND)
  return()
endif()

#SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/douf_java.jar)

ADD_JAR(douf_test_java 
  SOURCES src/Test.java
  INCLUDE_JARS douf_java)

SAFIR_GET_JAVA_CLASSPATH(
  JAVA_TARGETS douf_test_java douf_java dots_generated-Safir-java dots_java)

add_test(NAME TimeProvider_java_test 
  COMMAND ${Java_JAVA_EXECUTABLE} 
  -cp ${SAFIR_JAVA_CLASSPATH}
  Test)

SET_SAFIR_TEST_PROPERTIES(TEST TimeProvider_java_test
                          CONFIG_OVERRIDE "${CMAKE_CURRENT_SOURCE_DIR}/../config")
