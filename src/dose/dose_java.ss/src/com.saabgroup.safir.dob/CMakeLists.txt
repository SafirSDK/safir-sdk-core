file(GLOB_RECURSE sources src/*.java)

ADD_JAR(dose_java
  SOURCES ${sources}
  INCLUDE_JARS safir_generated-Core-java dots_java logging_java
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt
  GENERATE_NATIVE_HEADERS dose_java_jni_headers)

SAFIR_INSTALL(TARGETS dose_java)
