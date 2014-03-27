find_package(JNI REQUIRED)
string (REPLACE "javac" "javah" JAVAH ${JAVA_COMPILE})

INCLUDE_DIRECTORIES(${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2})

ADD_CUSTOM_COMMAND(OUTPUT 
  com_saabgroup_safir_Logging.h
  
  COMMAND ${JAVAH} -classpath ../com.saabgroup.safir/bin com.saabgroup.safir.Logging
  COMMENT "Generating javah header file"
  VERBATIM)

ADD_CUSTOM_TARGET(javah ALL
                  DEPENDS com_saabgroup_safir_Logging.h)

FILE(GLOB sources *.cpp)

ADD_LIBRARY(logging_java_jni SHARED ${sources})

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(logging_java_jni logging_library)
endif()

ADD_DEPENDENCIES(javah logging_java)
ADD_DEPENDENCIES(logging_java_jni javah)

INSTALL(TARGETS logging_java_jni
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(logging_java_jni)
