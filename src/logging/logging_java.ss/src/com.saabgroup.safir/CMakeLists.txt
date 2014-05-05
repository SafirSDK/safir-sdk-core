file(GLOB_RECURSE sources src/*.java)

ADD_JAR(logging_java SOURCES ${sources})

INSTALL_JAR(logging_java share/java/safir_sdk_core)
