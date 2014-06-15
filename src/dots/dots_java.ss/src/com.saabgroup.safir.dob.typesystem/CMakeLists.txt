file(GLOB_RECURSE sources src/*.java)

ADD_JAR(dots_java SOURCES ${sources})

INSTALL_JAR(dots_java share/java/safir_sdk_core)

SAFIR_JAVAH(JAR dots_java CLASS com.saabgroup.safir.dob.typesystem.Kernel
  OUTPUT_FILES
    com_saabgroup_safir_dob_typesystem_Kernel_DotsC_ErrorCode.h
    com_saabgroup_safir_dob_typesystem_Kernel.h
    com_saabgroup_safir_dob_typesystem_Kernel_DotsC_PropertyMappingKind.h
  OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/../dots_java_jni)
