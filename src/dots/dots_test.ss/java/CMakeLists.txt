if (NOT Java_FOUND)
  return()
endif()

#SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/dots_java.jar ${SAFIR_RUNTIME}/bin/dots_generated-java.jar)

#SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(dots_test_java 
  ENTRY_POINT Test
  SOURCES src/Test.java
  INCLUDE_JARS dots_generated-DotsTest-java dots_java)


