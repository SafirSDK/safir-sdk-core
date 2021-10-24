file(GLOB_RECURSE sources src/*.java)

ADD_JAR(swre_reports_java
  SOURCES ${sources}
  INCLUDE_JARS dots_java
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt
  GENERATE_NATIVE_HEADERS swre_reports_java_jni_headers )

SAFIR_INSTALL(TARGETS swre_reports_java)
