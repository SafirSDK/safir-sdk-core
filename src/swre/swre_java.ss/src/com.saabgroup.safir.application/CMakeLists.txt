file(GLOB_RECURSE sources src/*.java)

ADD_JAR(swre_application_java 
  SOURCES ${sources}
  INCLUDE_JARS 
  swre_reports_java 
  dose_java 
  safir_generated-Core-java
  dots_java
  logging_java
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

SAFIR_INSTALL(TARGETS swre_application_java)

SAFIR_JAVAH(JAR swre_application_java
  CLASS com.saabgroup.safir.application.Library
  OUTPUT_FILES com_saabgroup_safir_application_Library.h
  OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/../swre_java_jni)
