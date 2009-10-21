cmake_minimum_required(VERSION 2.6)

project(swre_test_java Java)
find_package(Java REQUIRED)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)
INCLUDE(JavaMacros)

file(GLOB_RECURSE sources src/*.java)

SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/dots_java.jar ${SAFIR_RUNTIME}/bin/dots_generated-java.jar ${SAFIR_RUNTIME}/bin/dose_java.jar ${SAFIR_RUNTIME}/bin/swre_reports_java.jar ${SAFIR_RUNTIME}/bin/swre_application_java.jar)

SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(swre_test_java "${sources}" ALL)

INSTALL(FILES swre_test_java.jar
  DESTINATION ${SAFIR_RUNTIME}/bin)

