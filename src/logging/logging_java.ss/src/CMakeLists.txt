if (NOT Java_FOUND)
  return()
endif()

#this project contains a library that has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

INCLUDE(JavaMacros)

add_subdirectory(com.saabgroup.safir)
add_subdirectory(logging_java_jni)
