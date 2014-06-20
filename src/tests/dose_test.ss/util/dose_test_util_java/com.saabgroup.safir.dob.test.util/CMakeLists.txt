file(GLOB_RECURSE sources src/*.java)

ADD_JAR(dose_test_util_java SOURCES ${sources})

INSTALL_JAR(dose_test_util_java share/java/safir_sdk_core)
#TODO put this path somewhere common?

SAFIR_JAVAH(JAR dose_test_util_java
  CLASS com.saabgroup.safir.dob.test.util.DoseTestUtil
  OUTPUT_FILES com_saabgroup_safir_dob_test_util_DoseTestUtil.h
  OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/../dose_test_util_java_jni)

