find_package(JNI REQUIRED)
string (REPLACE "javac" "javah" JAVAH ${JAVA_COMPILE})

INCLUDE_DIRECTORIES(${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2})

ADD_CUSTOM_COMMAND(OUTPUT 
  com_saabgroup_safir_dob_Interface.h
  
  COMMAND ${JAVAH} -classpath ../com.saabgroup.safir.dob/bin com.saabgroup.safir.dob.Interface
  COMMENT "Generating javah header files"
  VERBATIM)

ADD_CUSTOM_TARGET(javah ALL
                  DEPENDS com_saabgroup_safir_dob_Interface.h)

FILE(GLOB sources *.cpp)

ADD_LIBRARY(dose_java_jni SHARED ${sources})

if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dose_java_jni PRIVATE dose_dll)
endif()

ADD_DEPENDENCIES(javah dose_java)
ADD_DEPENDENCIES(dose_java_jni javah)

# INSTALL(TARGETS dose_java_jni
#         RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
#         LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
#         ARCHIVE DESTINATION ${SAFIR_SDK}/lib)

# INSTALL_DEBUG_INFO(dose_java_jni)
