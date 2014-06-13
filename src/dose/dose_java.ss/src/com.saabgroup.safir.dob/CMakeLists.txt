file(GLOB_RECURSE sources src/*.java)

ADD_JAR(dose_java 
  SOURCES ${sources}
  INCLUDE_JARS dots_generated-Safir-java dots_java logging_java
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

INSTALL_JAR(dose_java share/java/safir_sdk_core)
