file(GLOB_RECURSE sources src/*.java)

ADD_JAR(logging_java
  SOURCES ${sources})

SAFIR_INSTALL(TARGETS logging_java)

SAFIR_JAVAH(JAR logging_java CLASS com.saabgroup.safir.Logging
  OUTPUT_FILES
    com_saabgroup_safir_Logging.h
  OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/../logging_java_jni)
