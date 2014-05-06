if (NOT Java_FOUND)
  return()
endif()

INCLUDE(UseJava)

add_subdirectory(com.saabgroup.safir.dob)
add_subdirectory(dose_java_jni)
