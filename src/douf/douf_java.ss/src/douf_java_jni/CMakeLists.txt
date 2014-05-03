cmake_minimum_required(VERSION 2.8)

find_package(JNI REQUIRED)
string (REPLACE "javac" "javah" JAVAH ${JAVA_COMPILE})

INCLUDE_DIRECTORIES(${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2})

ADD_CUSTOM_COMMAND(OUTPUT 
  com_saabgroup_safir_time_Library.h

  COMMAND ${JAVAH} -classpath ../com.saabgroup.safir.time/bin com.saabgroup.safir.time.Library

  COMMENT "Generating javah header files"
  VERBATIM)

ADD_CUSTOM_TARGET(javah ALL
                  DEPENDS com_saabgroup_safir_time_Library.h)

ADD_LIBRARY(douf_java_jni SHARED douf_java_jni.cpp)


if (NOT MSVC)
    TARGET_LINK_LIBRARIES(douf_java_jni PRIVATE douf_time_library)
endif()

ADD_DEPENDENCIES(javah douf_java)
ADD_DEPENDENCIES(douf_java_jni javah)

# INSTALL(TARGETS douf_java_jni
#         RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
#         LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
#         ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

# INSTALL_DEBUG_INFO(douf_java_jni)
