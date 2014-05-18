#this project contains a library that has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

ADD_CUSTOM_COMMAND(OUTPUT 
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_Logging.h
  
  COMMAND ${Java_JAVAH_EXECUTABLE} -classpath $<TARGET_PROPERTY:logging_java,CLASSDIR> com.saabgroup.safir.Logging
  
  DEPENDS logging_java
  
  COMMENT "Generating javah header files for logging_java"
  VERBATIM)

ADD_LIBRARY(logging_java_jni SHARED 
  logging_jni.cpp
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_Logging.h)

TARGET_INCLUDE_DIRECTORIES(logging_java_jni PRIVATE ${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${CMAKE_CURRENT_BINARY_DIR})

TARGET_LINK_LIBRARIES(logging_java_jni PRIVATE logging_library)

SAFIR_INSTALL(TARGETS logging_java_jni)
