#this project contains a library that has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

ADD_LIBRARY(dose_java_jni SHARED
  dose_java_jni.cpp
  Callbacks.cpp
  ConsumerTable.cpp)

TARGET_INCLUDE_DIRECTORIES(dose_java_jni
  PRIVATE ${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${CMAKE_CURRENT_BINARY_DIR})

#depend on the javah target to ensure we get our header files.
add_dependencies(dose_java_jni dose_java_javah)

TARGET_LINK_LIBRARIES(dose_java_jni PRIVATE
  dose_dll
  dots_kernel
#  ${Boost_THREAD_LIBRARY}
#  ${Boost_SYSTEM_LIBRARY}
)
SAFIR_INSTALL(TARGETS dose_java_jni JNI)
