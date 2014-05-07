if (NOT Java_FOUND)
  return()
endif()

#SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/swre_application_java.jar)

#SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)
asdf
ADD_JAR(tracer_sender_java SOURCES Sender.java)

#ADD_TEST(tracer_java ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

#SET_SAFIR_TEST_PROPERTIES(TEST tracer_java TIMEOUT 120)
