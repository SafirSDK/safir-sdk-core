#this project contains a library that has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

ADD_CUSTOM_COMMAND(OUTPUT 
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_application_Library.h
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_swreports_Library.h
  
  COMMAND ${Java_JAVAH_EXECUTABLE} -classpath $<TARGET_PROPERTY:swre_application_java,CLASSDIR> com.saabgroup.safir.application.Library
  COMMAND ${Java_JAVAH_EXECUTABLE} -classpath $<TARGET_PROPERTY:swre_reports_java,CLASSDIR> com.saabgroup.safir.swreports.Library

  COMMENT "Generating javah header files")

ADD_LIBRARY(swre_java_jni SHARED 
  swre_jni.cpp
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_application_Library.h
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_swreports_Library.h)

TARGET_INCLUDE_DIRECTORIES(swre_java_jni PRIVATE ${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${CMAKE_CURRENT_BINARY_DIR})

TARGET_LINK_LIBRARIES(swre_java_jni PRIVATE 
  swre_library
  dots_cpp
  dots_internal
  lluf_utils)

SAFIR_INSTALL(TARGETS swre_java_jni)
