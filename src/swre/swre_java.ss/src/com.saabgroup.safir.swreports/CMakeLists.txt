file(GLOB_RECURSE sources src/*.java)

ADD_JAR(swre_reports_java
  SOURCES ${sources}
  INCLUDE_JARS dots_java
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

INSTALL_JAR(swre_reports_java ${SAFIR_INSTALL_DESTINATION_JAR})

SAFIR_JAVAH(JAR swre_reports_java
  CLASS com.saabgroup.safir.swreports.Library
  OUTPUT_FILES com_saabgroup_safir_swreports_Library.h
  OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/../swre_java_jni)
