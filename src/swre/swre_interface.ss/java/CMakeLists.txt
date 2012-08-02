cmake_minimum_required(VERSION 2.6)

if (NOT SAFIR_JAVA_SUPPORT)
     ADD_CUSTOM_TARGET(empty_target ALL
                       COMMENT "SAFIR_JAVA_SUPPORT set to false, swre_java_jni will not be built.")
     INSTALL(CODE "MESSAGE(\"SAFIR_JAVA_SUPPORT set to false, swre_java_jni will not be installed.\")")
     return()
endif (NOT SAFIR_JAVA_SUPPORT)

add_subdirectory(com.saabgroup.safir.swreports)
add_subdirectory(com.saabgroup.safir.application)
add_subdirectory(swre_java_jni)
