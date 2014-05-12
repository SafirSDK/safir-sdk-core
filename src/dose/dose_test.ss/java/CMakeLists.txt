if (NOT Java_FOUND)
  return()
endif()

file(GLOB_RECURSE sources src/*.java)

#SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(dose_test_java 
  SOURCES ${sources}
  INCLUDE_JARS 
  swre_application_java
  dose_test_util_java
  dose_java
  dots_generated-swre_library-java
  dots_generated-DoseTest-java 
  dots_generated-Safir-java
  dots_java)


INSTALL_JAR(dose_test_java share/java/safir_sdk_core)
