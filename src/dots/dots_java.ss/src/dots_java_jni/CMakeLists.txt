#this project contains a library that has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

ADD_CUSTOM_COMMAND(OUTPUT 
  com_saabgroup_safir_dob_typesystem_Kernel_DotsC_ErrorCode.h
  com_saabgroup_safir_dob_typesystem_Kernel.h
  com_saabgroup_safir_dob_typesystem_Kernel_DotsC_PropertyMappingKind.h

  COMMAND ${Java_JAVAH_EXECUTABLE} -classpath ../com.saabgroup.safir.dob.typesystem/dots_java.jar com.saabgroup.safir.dob.typesystem.Kernel

  COMMENT "Generating javah header files"
  VERBATIM)

ADD_CUSTOM_TARGET(dots_javah ALL
                  DEPENDS com_saabgroup_safir_dob_typesystem_Kernel.h)

ADD_LIBRARY(dots_java_jni SHARED dots_java_jni.cpp)

TARGET_INCLUDE_DIRECTORIES(dots_java_jni PRIVATE ${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2})

TARGET_LINK_LIBRARIES(dots_java_jni PRIVATE
  dots_kernel
  dots_internal
  lluf_id
  lluf_config)

ADD_DEPENDENCIES(dots_javah dots_java)
ADD_DEPENDENCIES(dots_java_jni dots_javah)

SAFIR_INSTALL(TARGETS dots_java_jni)
