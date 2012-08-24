cmake_minimum_required(VERSION 2.6)

if (NOT SAFIR_JAVA_SUPPORT)
     ADD_CUSTOM_TARGET(empty_target ALL
                       COMMENT "SAFIR_JAVA_SUPPORT set to false, dots_java will not be built.")
     INSTALL(CODE "MESSAGE(\"SAFIR_JAVA_SUPPORT set to false, dots_java will not be installed.\")")
     return()
endif (NOT SAFIR_JAVA_SUPPORT)

add_subdirectory(com.saabgroup.safir.dob.typesystem)
add_subdirectory(dots_java_jni)
