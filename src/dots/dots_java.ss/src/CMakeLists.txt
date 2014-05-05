if (NOT Java_FOUND)
  return()
endif()

INCLUDE(UseJava)

add_subdirectory(com.saabgroup.safir.dob.typesystem)
add_subdirectory(dots_java_jni)
