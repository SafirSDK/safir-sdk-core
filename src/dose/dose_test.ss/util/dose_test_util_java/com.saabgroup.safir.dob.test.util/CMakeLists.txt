file(GLOB_RECURSE sources src/*.java)

ADD_JAR(dose_test_util_java SOURCES ${sources})

INSTALL_JAR(dose_test_util_java share/java/safir_sdk_core)
#TODO put this path somewhere common?

