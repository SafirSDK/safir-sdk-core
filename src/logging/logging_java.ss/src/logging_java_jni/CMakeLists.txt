#this project contains a library that has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")



ADD_CUSTOM_COMMAND(OUTPUT 
  com_saabgroup_safir_Logging.h
  
  COMMAND ${Java_JAVAH_EXECUTABLE} -classpath ../com.saabgroup.safir/logging_java.jar com.saabgroup.safir.Logging
  COMMENT "Generating javah header file"
  VERBATIM)

ADD_CUSTOM_TARGET(logging_javah ALL
                  DEPENDS com_saabgroup_safir_Logging.h)

FILE(GLOB sources *.cpp)

ADD_LIBRARY(logging_java_jni SHARED ${sources})

TARGET_INCLUDE_DIRECTORIES(logging_java_jni PRIVATE ${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2})

TARGET_LINK_LIBRARIES(logging_java_jni PRIVATE logging_library)

ADD_DEPENDENCIES(logging_javah logging_java)
ADD_DEPENDENCIES(logging_java_jni logging_javah)

SAFIR_INSTALL(TARGETS logging_java_jni)
