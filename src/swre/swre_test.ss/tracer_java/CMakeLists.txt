find_package(Java REQUIRED)

INCLUDE(JavaMacros)

SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/swre_application_java.jar)

SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(tracer_sender_java Sender.java ALL)

ADD_TEST(tracer_java ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(TEST tracer_java TIMEOUT 120)
