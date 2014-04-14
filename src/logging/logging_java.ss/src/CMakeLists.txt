find_package(Java COMPONENTS Development Runtime REQUIRED)

#TODO 
#this project contains a library that has a C interface, so we don't want the "d" suffix on
#debug builds on windows. Must be set before including safir_sdk_core_config.cmake
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE(JavaMacros)

add_subdirectory(com.saabgroup.safir)
add_subdirectory(logging_java_jni)
