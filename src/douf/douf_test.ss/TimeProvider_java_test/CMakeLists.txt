if (NOT Java_FOUND)
  return()
endif()

ADD_JAR(douf_test_java
  SOURCES src/Test.java
  INCLUDE_JARS douf_java
  ENTRY_POINT Test
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_TEST(NAME TimeProvider_java_test
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/run_test.py
  --jar $<TARGET_PROPERTY:douf_test_java,JAR_FILE>)

SET_SAFIR_TEST_PROPERTIES(TEST TimeProvider_java_test)
