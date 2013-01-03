file(GLOB_RECURSE sources src/*.java)

SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/dots_java.jar
  ${SAFIR_RUNTIME}/bin/dots_generated-java.jar 
  ${SAFIR_RUNTIME}/bin/dose_java.jar 
  ${swre_interface_java_BINARY_DIR}/com.saabgroup.safir.swreports/swre_reports_java.jar)

SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(swre_application_java "${sources}" ALL)

INSTALL(FILES swre_application_java.jar
  DESTINATION ${SAFIR_RUNTIME}/bin)

ADD_DEPENDENCIES(swre_application_java swre_reports_java)
