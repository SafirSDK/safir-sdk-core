if (NOT Java_FOUND)
  return()
endif()

ADD_JAR(swreport_sender_java
  SOURCES Sender.java
  INCLUDE_JARS swre_reports_java
  ENTRY_POINT Sender
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_TEST(NAME swreport_java
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --jar $<TARGET_PROPERTY:swreport_sender_java,JAR_FILE>
  --safir-show-config $<TARGET_FILE:safir_show_config>)

SET_SAFIR_TEST_PROPERTIES(TEST swreport_java)
