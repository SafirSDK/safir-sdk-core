file(GLOB_RECURSE sources src/*.java)

ADD_JAR(swre_application_java 
  SOURCES ${sources}
  INCLUDE_JARS 
  swre_reports_java 
  dose_java 
  dots_generated-swre_library-java
  dots_generated-Safir-java
  dots_java
  logging_java
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

INSTALL_JAR(swre_application_java share/java/safir_sdk_core)
