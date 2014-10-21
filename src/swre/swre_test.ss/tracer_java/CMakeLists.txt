if (NOT Java_FOUND)
  return()
endif()

ADD_JAR(tracer_sender_java
  SOURCES Sender.java
  INCLUDE_JARS swre_application_java
  ENTRY_POINT Sender
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_TEST(NAME tracer_java
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --jar $<TARGET_PROPERTY:tracer_sender_java,JAR_FILE>
  --dependencies $<TARGET_PROPERTY:swre_application_java,JAR_FILE>
  --safir-show-config $<TARGET_FILE:safir_show_config>)

SET_SAFIR_TEST_PROPERTIES(TEST tracer_java TIMEOUT 120)
