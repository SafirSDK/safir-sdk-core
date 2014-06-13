file(GLOB_RECURSE sources src/*.java)

ADD_JAR(douf_java 
  SOURCES ${sources}
  INCLUDE_JARS dots_generated-Safir-java dots_java
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

INSTALL_JAR(douf_java share/java/safir_sdk_core)
