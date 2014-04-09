find_package(Java REQUIRED)

INCLUDE(JavaMacros)

file(GLOB_RECURSE sources *.java)

SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/logging_java.jar)

SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(log_sender_java "${sources}" ALL)

ADD_TEST(logging_java ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py)

SET_SAFIR_TEST_PROPERTIES(TEST logging_java TIMEOUT 120)
