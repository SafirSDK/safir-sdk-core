file(GLOB_RECURSE sources src/*.java)

ADD_JAR(dots_java
  SOURCES ${sources}
  INCLUDE_JARS logging_java
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt
  GENERATE_NATIVE_HEADERS dots_java_jni_headers)

SAFIR_INSTALL(TARGETS dots_java)
