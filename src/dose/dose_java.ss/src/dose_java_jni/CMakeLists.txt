ADD_LIBRARY(dose_java_jni SHARED
  dose_java_jni.cpp
  Callbacks.cpp
  ConsumerTable.cpp)
safir_mark_single_abi(dose_java_jni)

TARGET_INCLUDE_DIRECTORIES(dose_java_jni PRIVATE
  ${JAVA_INCLUDE_PATH}
  ${JAVA_INCLUDE_PATH2}
  ${CMAKE_CURRENT_BINARY_DIR})

TARGET_LINK_LIBRARIES(dose_java_jni PRIVATE
  dose_dll
  dots_kernel
  Boost::thread
  dose_java_jni_headers)

SAFIR_INSTALL(TARGETS dose_java_jni JNI)
