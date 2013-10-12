file(GLOB_RECURSE sources src/*.java)

ADD_JAR(logging_java "${sources}" ALL)

INSTALL(FILES logging_java.jar
  DESTINATION ${SAFIR_RUNTIME}/bin)
