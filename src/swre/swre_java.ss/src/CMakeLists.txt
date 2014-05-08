if (NOT Java_FOUND)
  return()
endif()

add_subdirectory(com.saabgroup.safir.swreports)
add_subdirectory(com.saabgroup.safir.application)
add_subdirectory(swre_java_jni)
