cmake_minimum_required(VERSION 2.8)

project(swre_reports_java Java)
find_package(Java REQUIRED)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)
INCLUDE(JavaMacros)

file(GLOB_RECURSE sources src/*.java)

SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/dots_java.jar)

SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(swre_reports_java "${sources}" ALL)

INSTALL(FILES swre_reports_java.jar
  DESTINATION ${SAFIR_RUNTIME}/bin)
