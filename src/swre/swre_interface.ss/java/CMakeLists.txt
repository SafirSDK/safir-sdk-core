cmake_minimum_required(VERSION 2.8)

project(swre_interface_java CXX)

if (NOT SAFIR_JAVA_SUPPORT)
     ADD_CUSTOM_TARGET(empty_target ALL
                       COMMENT "SAFIR_JAVA_SUPPORT set to false, swre_java_jni will not be built.")
     INSTALL(CODE "MESSAGE(\"SAFIR_JAVA_SUPPORT set to false, swre_java_jni will not be installed.\")")
     return()
endif ()

find_package(Java COMPONENTS Development Runtime REQUIRED)

#this project contains a library that has a C interface, so we don't want the "d" suffix on 
#debug builds on windows. Must be set before including cmake_settings.txt
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)
INCLUDE(JavaMacros)

add_subdirectory(com.saabgroup.safir.swreports)
add_subdirectory(com.saabgroup.safir.application)
add_subdirectory(swre_java_jni)
