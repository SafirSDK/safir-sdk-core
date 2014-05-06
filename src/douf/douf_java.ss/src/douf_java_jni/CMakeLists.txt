#this project contains a library that has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

ADD_CUSTOM_COMMAND(OUTPUT 
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_time_Library.h

  COMMAND ${Java_JAVAH_EXECUTABLE} -classpath ../com.saabgroup.safir.time/douf_java.jar com.saabgroup.safir.time.Library

  DEPENDS douf_java

  COMMENT "Generating javah header files"
  VERBATIM)

ADD_LIBRARY(douf_java_jni SHARED 
  douf_java_jni.cpp
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_time_Library.h)

TARGET_INCLUDE_DIRECTORIES(douf_java_jni PRIVATE ${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${CMAKE_CURRENT_BINARY_DIR})

TARGET_LINK_LIBRARIES(douf_java_jni PRIVATE
  douf_time_library
  dots_internal)

SAFIR_INSTALL(TARGETS douf_java_jni)
