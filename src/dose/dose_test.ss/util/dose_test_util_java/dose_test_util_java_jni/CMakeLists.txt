#this library has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

ADD_CUSTOM_COMMAND(OUTPUT 
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_dob_test_util_DoseTestUtil.h
  
  COMMAND ${Java_JAVAH_EXECUTABLE} -classpath ../com.saabgroup.safir.dob.test.util/dose_test_util_java.jar com.saabgroup.safir.dob.test.util.DoseTestUtil
  
  DEPENDS dose_test_util_java
  
  COMMENT "Generating javah header files"
  VERBATIM)

ADD_LIBRARY(dose_test_util_java_jni SHARED
  dose_test_util_java_jni.cpp
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_dob_test_util_DoseTestUtil.h)


TARGET_INCLUDE_DIRECTORIES(dose_test_util_java_jni PRIVATE ${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${CMAKE_CURRENT_BINARY_DIR})

TARGET_LINK_LIBRARIES(dose_test_util_java_jni PRIVATE dose_test_util)

SAFIR_INSTALL(TARGETS dose_test_util_java_jni)
