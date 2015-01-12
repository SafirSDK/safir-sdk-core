file(GLOB_RECURSE sources src/*.java)

ADD_JAR(douf_java
  SOURCES ${sources}
  INCLUDE_JARS safir_generated-Core-java dots_java
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

SAFIR_INSTALL(TARGETS douf_java)

SAFIR_JAVAH(JAR douf_java
  CLASS com.saabgroup.safir.time.Library
  OUTPUT_FILES com_saabgroup_safir_time_Library.h
  OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/../douf_java_jni)
