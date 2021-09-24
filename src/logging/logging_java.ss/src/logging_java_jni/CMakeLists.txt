#this project contains a library that has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

ADD_LIBRARY(logging_java_jni SHARED
  logging_jni.cpp)

TARGET_INCLUDE_DIRECTORIES(logging_java_jni
  PRIVATE ${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${CMAKE_CURRENT_BINARY_DIR})

TARGET_LINK_LIBRARIES(logging_java_jni PRIVATE logging_library logging_java_jni_headers)

SAFIR_INSTALL(TARGETS logging_java_jni JNI)
