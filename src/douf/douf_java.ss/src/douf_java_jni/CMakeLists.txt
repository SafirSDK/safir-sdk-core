ADD_LIBRARY(douf_java_jni SHARED
  douf_java_jni.cpp)
safir_mark_single_abi(douf_java_jni)

TARGET_INCLUDE_DIRECTORIES(douf_java_jni PRIVATE
  ${JAVA_INCLUDE_PATH}
  ${JAVA_INCLUDE_PATH2}
  ${CMAKE_CURRENT_BINARY_DIR})

TARGET_LINK_LIBRARIES(douf_java_jni PRIVATE
  douf_time_library
  douf_java_jni_headers)

SAFIR_INSTALL(TARGETS douf_java_jni JNI)
