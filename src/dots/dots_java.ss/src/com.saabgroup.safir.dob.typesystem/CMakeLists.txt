file(GLOB_RECURSE sources src/*.java)

ADD_JAR(dots_java SOURCES ${sources})

INSTALL_JAR(dots_java share/java/safir_sdk_core)
