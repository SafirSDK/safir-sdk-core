if (NOT Java_FOUND)
  return()
endif()

ADD_JAR(swreport_sender_java 
  SOURCES Sender.java
  INCLUDE_JARS swre_reports_java
  ENTRY_POINT Sender
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

SAFIR_GET_JAVA_CLASSPATH(
  JAVA_TARGETS swreport_sender_java swre_reports_java)

ADD_TEST(NAME swreport_java
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --jar $<TARGET_PROPERTY:swreport_sender_java,JAR_FILE>
  --dependencies $<TARGET_PROPERTY:swre_reports_java,JAR_FILE>
  --safir-show-config $<TARGET_FILE:safir_show_config>)

SET_SAFIR_TEST_PROPERTIES(TEST swreport_java TIMEOUT 120)
