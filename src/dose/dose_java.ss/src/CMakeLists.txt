cmake_minimum_required(VERSION 2.8)

if (NOT SAFIR_JAVA_SUPPORT)
     ADD_CUSTOM_TARGET(empty_target ALL
                       COMMENT "SAFIR_JAVA_SUPPORT set to false, dose_java will not be built.")
     INSTALL(CODE "MESSAGE(\"SAFIR_JAVA_SUPPORT set to false, dose_java will not be installed.\")")
     return()
endif (NOT SAFIR_JAVA_SUPPORT)

add_subdirectory(com.saabgroup.safir.dob)
add_subdirectory(dose_java_jni)
