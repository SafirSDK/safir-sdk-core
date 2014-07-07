cmake_minimum_required(VERSION 2.8)

project(vehicleappjava Java)
find_package(Java REQUIRED)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)
INCLUDE(JavaMacros)

file(GLOB_RECURSE sources com/*.java)

SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/dose_java.jar ${SAFIR_RUNTIME}/bin/logging_java.jar)

SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(vehicleappjava "${sources}" ALL)

INSTALL(FILES vehicleappjava.jar
  DESTINATION ${SAFIR_RUNTIME}/bin)
