if (NOT Java_FOUND)
  return()
endif()

ADD_JAR(dots_test_java
  SOURCES src/Test.java
  INCLUDE_JARS safir_generated-DotsTest-java dots_java
  ENTRY_POINT Test
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_DEPENDENCIES(dots_test_java safir_generated-DotsTestExtra-java)
