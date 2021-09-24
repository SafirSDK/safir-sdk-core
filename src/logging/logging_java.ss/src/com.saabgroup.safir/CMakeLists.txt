file(GLOB_RECURSE sources src/*.java)

ADD_JAR(logging_java
  SOURCES ${sources}
  GENERATE_NATIVE_HEADERS logging_java_jni_headers)

SAFIR_INSTALL(TARGETS logging_java)
