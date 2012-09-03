cmake_minimum_required(VERSION 2.8)

project(dob_web_server Java)
find_package(Java REQUIRED)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)
INCLUDE(JavaMacros)

file(GLOB_RECURSE sources src/*.java)

SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/dots_java.jar ${SAFIR_RUNTIME}/bin/dots_generated-java.jar ${SAFIR_RUNTIME}/bin/dose_java.jar )

SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(dob_web_server "${sources}" ALL)

INSTALL(FILES dob_web_server.jar
  DESTINATION ${SAFIR_RUNTIME}/bin)

