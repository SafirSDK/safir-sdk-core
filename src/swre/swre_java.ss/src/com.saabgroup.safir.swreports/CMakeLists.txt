file(GLOB_RECURSE sources src/*.java)

ADD_JAR(swre_reports_java
  SOURCES ${sources}
  INCLUDE_JARS dots_java
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

INSTALL_JAR(swre_reports_java share/java/safir_sdk_core)
