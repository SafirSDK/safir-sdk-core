#this project contains a library that has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

#we need to disable some warnings, since we do some wierd things with pointers
if (NOT MSVC)
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-pedantic")
endif()


ADD_LIBRARY(dots_java_jni SHARED
  dots_java_jni.cpp)

TARGET_INCLUDE_DIRECTORIES(dots_java_jni PRIVATE
  ${JAVA_INCLUDE_PATH}
  ${JAVA_INCLUDE_PATH2}
  ${CMAKE_CURRENT_BINARY_DIR})

#depend on the javah target to ensure we get our header files.
add_dependencies(dots_java_jni dots_java_javah)

TARGET_LINK_LIBRARIES(dots_java_jni PRIVATE
  lluf_config
  lluf_id
  dots_kernel
  ${Boost_FILESYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS dots_java_jni JNI)
