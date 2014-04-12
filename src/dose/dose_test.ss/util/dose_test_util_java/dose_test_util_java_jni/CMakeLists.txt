project(dose_test_util_java_jni Java CXX)
find_package(Java REQUIRED)
find_package(JNI REQUIRED)
string (REPLACE "javac" "javah" JAVAH ${JAVA_COMPILE})

#this library has a C interface, so we don't want the "d" suffix on debug builds on windows
# NO_LIBRARY_POSTFIXES 1 is set in the top level CMakeLists.txt file

INCLUDE_DIRECTORIES(${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${dose_test_util_SOURCE_DIR})

#we link explicitly, so disable autolinking
ADD_DEFINITIONS(-DDOSE_TEST_NO_AUTOLINK)

ADD_CUSTOM_COMMAND(OUTPUT 
  com_saabgroup_safir_dob_test_util_DoseTestUtil.h
  
  COMMAND ${JAVAH} -classpath ../com.saabgroup.safir.dob.test.util/bin com.saabgroup.safir.dob.test.util.DoseTestUtil
  
  COMMENT "Generating javah header files"
  VERBATIM)

ADD_CUSTOM_TARGET(javah ALL
                  DEPENDS com_saabgroup_safir_dob_test_util_DoseTestUtil.h)

ADD_LIBRARY(dose_test_util_java_jni SHARED dose_test_util_java_jni.cpp)

TARGET_LINK_LIBRARIES(dose_test_util_java_jni PRIVATE dose_test_util)

ADD_DEPENDENCIES(javah dose_test_util_java)
ADD_DEPENDENCIES(dose_test_util_java_jni javah)

INSTALL(TARGETS dose_test_util_java_jni
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(dose_test_util_java_jni)
