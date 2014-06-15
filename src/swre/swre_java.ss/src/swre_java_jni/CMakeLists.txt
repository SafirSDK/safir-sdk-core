#this project contains a library that has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

ADD_LIBRARY(swre_java_jni SHARED 
  swre_jni.cpp)

TARGET_INCLUDE_DIRECTORIES(swre_java_jni PRIVATE ${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${CMAKE_CURRENT_BINARY_DIR})

#depend on the javah targets to ensure we get our header files.
add_dependencies(swre_java_jni swre_application_java_javah swre_reports_java_javah)

TARGET_LINK_LIBRARIES(swre_java_jni PRIVATE 
  swre_library)

SAFIR_INSTALL(TARGETS swre_java_jni)
