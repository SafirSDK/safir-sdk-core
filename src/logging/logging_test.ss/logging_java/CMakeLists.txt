if (NOT Java_FOUND)
  return()
endif()

ADD_JAR(log_sender_java
  SOURCES Sender.java
  INCLUDE_JARS logging_java
  ENTRY_POINT Sender
  VERSION ${SAFIR_VERSION_STRING}
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_TEST(NAME logging_java
  COMMAND ${PYTHON_EXECUTABLE}
  ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --jar $<TARGET_PROPERTY:log_sender_java,JAR_FILE>
  --safir-show-config $<TARGET_FILE:safir_show_config>
  --dependencies $<TARGET_PROPERTY:logging_java,JAR_FILE>)

SET_SAFIR_TEST_PROPERTIES(TEST logging_java TIMEOUT 120)
