file(GLOB_RECURSE sources src/*.java)

ADD_JAR(swre_application_java
  SOURCES ${sources}
  INCLUDE_JARS
  swre_reports_java
  dose_java
  safir_generated-Core-java
  dots_java
  logging_java
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt
  GENERATE_NATIVE_HEADERS swre_application_java_jni_headers)

SAFIR_INSTALL(TARGETS swre_application_java)

