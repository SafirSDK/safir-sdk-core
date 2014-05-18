#this project contains a library that has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

ADD_CUSTOM_COMMAND(OUTPUT 
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_dob_typesystem_Kernel_DotsC_ErrorCode.h
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_dob_typesystem_Kernel.h
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_dob_typesystem_Kernel_DotsC_PropertyMappingKind.h

  COMMAND ${Java_JAVAH_EXECUTABLE} -classpath $<TARGET_PROPERTY:dots_java,CLASSDIR> com.saabgroup.safir.dob.typesystem.Kernel
  
  COMMENT "Generating javah header files for dots_java_jni")

ADD_LIBRARY(dots_java_jni SHARED 
  dots_java_jni.cpp
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_dob_typesystem_Kernel_DotsC_ErrorCode.h
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_dob_typesystem_Kernel.h
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_dob_typesystem_Kernel_DotsC_PropertyMappingKind.h)

TARGET_INCLUDE_DIRECTORIES(dots_java_jni PRIVATE ${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${CMAKE_CURRENT_BINARY_DIR})

TARGET_LINK_LIBRARIES(dots_java_jni PRIVATE
  dots_kernel
  dots_internal
  lluf_id
  lluf_config
  ${Boost_FILESYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS dots_java_jni)


#TODO: make the headers regenerate upon change of a java file, e.g. Kernel.java. Doesnt work now for some reason!
