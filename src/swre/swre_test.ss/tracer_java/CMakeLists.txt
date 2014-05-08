if (NOT Java_FOUND)
  return()
endif()

#SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/swre_application_java.jar)

#SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(tracer_sender_java 
  SOURCES Sender.java
  INCLUDE_JARS swre_application_java)

SAFIR_GET_JAVA_CLASSPATH_AND_LIBPATH(
  JAVA_TARGETS tracer_sender_java swre_application_java
  JNI_TARGETS swre_java_jni)

ADD_TEST(NAME tracer_java
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --classpath ${SAFIR_JAVA_CLASSPATH}
  --library-path ${SAFIR_JAVA_LIBRARY_PATH}
  --safir-show-config $<TARGET_FILE:safir_show_config>)

SET_SAFIR_TEST_PROPERTIES(TEST tracer_java TIMEOUT 120)
