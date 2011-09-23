cmake_minimum_required(VERSION 2.6)

project(dose_test_util_java_jni Java)
find_package(Java REQUIRED)
find_package(JNI REQUIRED)
string (REPLACE "javac" "javah" JAVAH ${JAVA_COMPILE})

#this library has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including cmake_settings.txt
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

INCLUDE_DIRECTORIES(${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2})

ADD_CUSTOM_COMMAND(OUTPUT 
  com_saabgroup_safir_dob_test_util_DoseTestUtil.h

  COMMAND ${JAVAH} -classpath ../com.saabgroup.safir.dob.test.util/bin com.saabgroup.safir.dob.test.util.DoseTestUtil

  COMMENT "Generating javah header files"
  VERBATIM)

ADD_CUSTOM_TARGET(javah ALL
                  DEPENDS com_saabgroup_safir_dob_test_util_DoseTestUtil.h)

ADD_LIBRARY(dose_test_util_java_jni SHARED dose_test_util_java_jni.cpp)


if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dose_test_util_java_jni dose_test_util)
endif()

ADD_DEPENDENCIES(javah dose_test_util_java)
ADD_DEPENDENCIES(dose_test_util_java_jni javah)

INSTALL(TARGETS dose_test_util_java_jni
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dose_test_util_java_jni)