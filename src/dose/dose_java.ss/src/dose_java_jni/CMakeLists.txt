#this project contains a library that has a C interface and we only want one version of it,
#so we never want the "d" suffix on debug builds, which is default on windows.
SET(CMAKE_DEBUG_POSTFIX "")

ADD_CUSTOM_COMMAND(OUTPUT 
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_dob_Interface.h
  
  COMMAND ${Java_JAVAH_EXECUTABLE} -classpath $<TARGET_PROPERTY:dose_java,CLASSDIR> com.saabgroup.safir.dob.Interface

  COMMENT "Generating javah header files for dose_java_jni")


ADD_LIBRARY(dose_java_jni SHARED 
  dose_java_jni.cpp
  Callbacks.cpp
  ConsumerTable.cpp
  ${CMAKE_CURRENT_BINARY_DIR}/com_saabgroup_safir_dob_Interface.h)

TARGET_INCLUDE_DIRECTORIES(dose_java_jni PRIVATE ${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${CMAKE_CURRENT_BINARY_DIR})

TARGET_LINK_LIBRARIES(dose_java_jni PRIVATE 
  dose_dll
  dots_kernel
#  dots_internal
  ${Boost_THREAD_LIBRARY}
  ${Boost_SYSTEM_LIBRARY})

SAFIR_INSTALL(TARGETS dose_java_jni)
