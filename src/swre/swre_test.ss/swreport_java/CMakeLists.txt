find_package(Java REQUIRED)

INCLUDE(JavaMacros)

SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/swre_reports_java.jar)

SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(swreport_sender_java Sender.java ALL)

ADD_TEST(swreport_java ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(TEST swreport_java TIMEOUT 120)
