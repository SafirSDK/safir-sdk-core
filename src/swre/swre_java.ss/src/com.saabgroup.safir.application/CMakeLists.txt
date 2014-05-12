file(GLOB_RECURSE sources src/*.java)

#SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/dots_java.jar
#  ${SAFIR_RUNTIME}/bin/dots_generated-java.jar 
#  ${SAFIR_RUNTIME}/bin/dose_java.jar 
#  ${SAFIR_RUNTIME}/bin/swre_application_java.jar)

#SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(swre_application_java 
  SOURCES ${sources}
  INCLUDE_JARS 
  swre_reports_java 
  dose_java 
  dots_generated-swre_library-java
  dots_generated-Safir-java
  dots_java
  logging_java)

INSTALL_JAR(swre_application_java share/java/safir_sdk_core)
