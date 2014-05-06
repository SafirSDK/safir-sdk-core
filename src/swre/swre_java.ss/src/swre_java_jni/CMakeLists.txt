#this project contains a library that has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

ADD_CUSTOM_COMMAND(OUTPUT 
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_application_Library.h
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_swreports_Library.h
  
  COMMAND ${JAVAH} -classpath ../com.saabgroup.safir.application/swre_application_java.jar com.saabgroup.safir.application.Library
  COMMAND ${JAVAH} -classpath ../com.saabgroup.safir.swreports/swre_swreports_java.jar com.saabgroup.safir.swreports.Library

  DEPENDS swre_application_java swre_swreports_java

  COMMENT "Generating javah header files"
  VERBATIM)

ADD_LIBRARY(swre_java_jni SHARED 
  swre_jni.cpp
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_application_Library.h
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_swreports_Library.h)

TARGET_INCLUDE_DIRECTORIES(swre_java_jni PRIVATE ${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${CMAKE_CURRENT_BINARY_DIR})

TARGET_LINK_LIBRARIES(swre_java_jni PRIVATE swre_library)

SAFIR_INSTALL(TARGETS swre_java_jni)
