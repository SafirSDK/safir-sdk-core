#this project contains a library that has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

ADD_LIBRARY(dots_java_jni SHARED
  dots_java_jni.cpp)

TARGET_INCLUDE_DIRECTORIES(dots_java_jni PRIVATE
  ${JAVA_INCLUDE_PATH}
  ${JAVA_INCLUDE_PATH2}
  ${CMAKE_CURRENT_BINARY_DIR})

TARGET_LINK_LIBRARIES(dots_java_jni PRIVATE
  lluf_config
  lluf_id
  dots_kernel
  Boost::filesystem
  dots_java_jni_headers)

SAFIR_INSTALL(TARGETS dots_java_jni JNI)
