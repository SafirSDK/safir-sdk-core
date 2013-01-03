cmake_minimum_required(VERSION 2.8)

find_package(JNI REQUIRED)
string (REPLACE "javac" "javah" JAVAH ${JAVA_COMPILE})

INCLUDE_DIRECTORIES(${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2})

ADD_CUSTOM_COMMAND(OUTPUT 
  com_saabgroup_safir_dob_typesystem_Kernel_DotsC_ErrorCode.h
  com_saabgroup_safir_dob_typesystem_Kernel.h
  com_saabgroup_safir_dob_typesystem_Kernel_DotsC_PropertyMappingKind.h

  COMMAND ${JAVAH} -classpath ../com.saabgroup.safir.dob.typesystem/bin com.saabgroup.safir.dob.typesystem.Kernel

  COMMENT "Generating javah header files"
  VERBATIM)

ADD_CUSTOM_TARGET(javah ALL
                  DEPENDS com_saabgroup_safir_dob_typesystem_Kernel.h)

ADD_LIBRARY(dots_java_jni SHARED dots_java_jni.cpp)


if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dots_java_jni dots_kernel)
endif()

ADD_DEPENDENCIES(javah dots_java)
ADD_DEPENDENCIES(dots_java_jni javah)

INSTALL(TARGETS dots_java_jni
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dots_java_jni)
