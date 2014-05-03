cmake_minimum_required(VERSION 2.8)

file(GLOB_RECURSE sources src/*.java)

SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/dots_java.jar ${SAFIR_RUNTIME}/bin/dots_generated-java.jar ${SAFIR_RUNTIME}/bin/logging_java.jar)

SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(dose_java "${sources}" ALL)

# INSTALL(FILES dose_java.jar
#   DESTINATION ${SAFIR_RUNTIME}/bin)
