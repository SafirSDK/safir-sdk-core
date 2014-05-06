if (NOT Java_FOUND)
  return()
endif()

INCLUDE(UseJava)

add_subdirectory(com.saabgroup.safir.time)
add_subdirectory(douf_java_jni)
