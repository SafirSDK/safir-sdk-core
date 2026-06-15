ADD_LIBRARY(dots_java_jni SHARED
  dots_java_jni.cpp)
safir_mark_single_abi(dots_java_jni)

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
