file(GLOB_RECURSE sources src/*.java)

#SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/dots_java.jar ${SAFIR_RUNTIME}/bin/dots_generated-java.jar ${SAFIR_RUNTIME}/bin/logging_java.jar)

#SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(dose_java 
  SOURCES ${sources}
  INCLUDE_JARS dots_generated-Safir-java dots_java logging_java)

INSTALL_JAR(dose_java share/java/safir_sdk_core)


#TODO: remove all unused Manifest files
