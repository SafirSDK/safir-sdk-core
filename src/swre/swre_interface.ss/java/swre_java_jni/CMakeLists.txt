cmake_minimum_required(VERSION 2.6)

project(swre_java_jni Java)
find_package(Java REQUIRED)
find_package(JNI REQUIRED)
string (REPLACE "javac" "javah" JAVAH ${JAVA_COMPILE})

#this library has a C interface, so we don't want the "d" suffix on debug builds on windows
#Must be set before including cmake_settings.txt
SET(NO_LIBRARY_POSTFIXES 1) 

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

INCLUDE_DIRECTORIES(${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2})

ADD_CUSTOM_COMMAND(OUTPUT 
  com_saabgroup_safir_application_Library.h
  com_saabgroup_safir_swreports_Library.h
  
  COMMAND ${JAVAH} -classpath ../com.saabgroup.safir.application/bin com.saabgroup.safir.application.Library
  COMMAND ${JAVAH} -classpath ../com.saabgroup.safir.swreports/bin com.saabgroup.safir.swreports.Library
  COMMENT "Generating javah header files"
  VERBATIM)

ADD_CUSTOM_TARGET(javah ALL
                  DEPENDS com_saabgroup_safir_application_Library.h com_saabgroup_safir_swreports_Library.h)

FILE(GLOB sources *.cpp)

ADD_LIBRARY(swre_java_jni SHARED ${sources})

TARGET_LINK_LIBRARIES(swre_java_jni ${ACE_LIBRARY})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(swre_java_jni swre_library)
endif()

ADD_DEPENDENCIES(javah swre_reports_java swre_application_java)
ADD_DEPENDENCIES(swre_java_jni javah)

INSTALL(TARGETS swre_java_jni
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(swre_java_jni)