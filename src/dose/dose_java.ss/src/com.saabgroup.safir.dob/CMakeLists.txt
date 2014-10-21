file(GLOB_RECURSE sources src/*.java)

ADD_JAR(dose_java
  SOURCES ${sources}
  INCLUDE_JARS safir_generated-Core-java dots_java logging_java
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

SAFIR_INSTALL(TARGETS dose_java)

SAFIR_JAVAH(JAR dose_java
  CLASS com.saabgroup.safir.dob.Interface
  OUTPUT_FILES com_saabgroup_safir_dob_Interface.h
  OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/../dose_java_jni)
