if (NOT Java_FOUND)
  return()
endif()

INCLUDE(UseJava)

add_subdirectory(com.saabgroup.safir)
add_subdirectory(logging_java_jni)
