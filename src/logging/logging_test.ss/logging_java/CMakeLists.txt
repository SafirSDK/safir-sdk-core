if (NOT Java_FOUND)
  return()
endif()

INCLUDE(UseJava)
#SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/logging_java.jar)

#SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(log_sender_java 
  SOURCES Sender.java
  INCLUDE_JARS logging_java)

SET_JAVA_CLASSPATH(TARGETS log_sender_java logging_java)

message("CP: ${SAFIR_JAVA_CLASSPATH}")

ADD_TEST(NAME logging_java
  COMMAND ${PYTHON_EXECUTABLE}
  ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --classpath ${SAFIR_JAVA_CLASSPATH}
  --safir-show-config $<TARGET_FILE:safir_show_config>)

SET_SAFIR_TEST_PROPERTIES(TEST logging_java TIMEOUT 120)
