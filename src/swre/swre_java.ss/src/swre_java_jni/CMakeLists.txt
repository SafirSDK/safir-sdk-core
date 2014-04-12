find_package(JNI REQUIRED)
string (REPLACE "javac" "javah" JAVAH ${JAVA_COMPILE})

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

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(swre_java_jni PRIVATE swre_library)
endif()

ADD_DEPENDENCIES(javah swre_reports_java swre_application_java)
ADD_DEPENDENCIES(swre_java_jni javah)

INSTALL(TARGETS swre_java_jni
        RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
        LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
        ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

INSTALL_DEBUG_INFO(swre_java_jni)
