file(GLOB_RECURSE sources src/*.java)


#SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/dots_java.jar ${SAFIR_RUNTIME}/bin/dots_generated-java.jar)

#SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(douf_java 
  SOURCES ${sources}
  INCLUDE_JARS dots_generated-Safir-java dots_java)

INSTALL_JAR(douf_java share/java/safir_sdk_core)
