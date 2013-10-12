cmake_minimum_required(VERSION 2.8)

project(dots_java CXX)

if (NOT SAFIR_JAVA_SUPPORT)
     ADD_CUSTOM_TARGET(empty_target ALL
                       COMMENT "SAFIR_JAVA_SUPPORT set to false, dots_java will not be built.")
     INSTALL(CODE "MESSAGE(\"SAFIR_JAVA_SUPPORT set to false, dots_java will not be installed.\")")
     return()
endif ()

find_package(Java REQUIRED)

#this project contains a library that has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including safir_sdk_core_config.cmake
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)
INCLUDE(JavaMacros)

add_subdirectory(com.saabgroup.safir.dob.typesystem)
add_subdirectory(dots_java_jni)
