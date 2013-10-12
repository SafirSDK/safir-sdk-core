file(GLOB_RECURSE sources src/*.java)

ADD_JAR(dots_java "${sources}" ALL)

INSTALL(FILES dots_java.jar
  DESTINATION ${SAFIR_RUNTIME}/bin)
