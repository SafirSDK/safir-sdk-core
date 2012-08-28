cmake_minimum_required(VERSION 2.6)

if (NOT SAFIR_JAVA_SUPPORT)
     ADD_CUSTOM_TARGET(empty_dose_test_java ALL
                       COMMENT "SAFIR_JAVA_SUPPORT set to false, dose_test_java will not be built.")
     INSTALL(CODE "MESSAGE(\"SAFIR_JAVA_SUPPORT set to false, dose_test_java will not be installed.\")")
     return()
endif (NOT SAFIR_JAVA_SUPPORT)

project(dose_test_java Java)
find_package(Java REQUIRED)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)
INCLUDE(JavaMacros)

file(GLOB_RECURSE sources src/*.java)

get_target_property(UTIL_LOCATION dose_test_util_java LOCATION )
if (NOT UTIL_LOCATION)
  SET(UTIL_LOCATION ${SAFIR_RUNTIME}/bin/dose_test_util_java.jar)
endif()


SET_JAVA_CLASSPATH(${UTIL_LOCATION} ${SAFIR_RUNTIME}/bin/dots_java.jar ${SAFIR_RUNTIME}/bin/dots_generated-java.jar ${SAFIR_RUNTIME}/bin/dose_java.jar ${SAFIR_RUNTIME}/bin/swre_application_java.jar)

SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)


ADD_JAR(dose_test_java "${sources}" ALL)



INSTALL(FILES dose_test_java.jar
  DESTINATION ${SAFIR_RUNTIME}/bin)

