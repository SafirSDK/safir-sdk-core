ADD_LIBRARY(swre_java_jni SHARED
  swre_jni.cpp)
safir_mark_single_abi(swre_java_jni)

TARGET_INCLUDE_DIRECTORIES(swre_java_jni
  PRIVATE
  ${JAVA_INCLUDE_PATH}
  ${JAVA_INCLUDE_PATH2}
  ${CMAKE_CURRENT_BINARY_DIR})

TARGET_LINK_LIBRARIES(swre_java_jni PRIVATE
  swre_library
  swre_reports_java_jni_headers
  swre_application_java_jni_headers)

SAFIR_INSTALL(TARGETS swre_java_jni JNI)
