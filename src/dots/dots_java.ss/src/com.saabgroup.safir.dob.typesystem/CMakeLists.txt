cmake_minimum_required(VERSION 2.6)

project(dots_java Java)
find_package(Java REQUIRED)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)
INCLUDE(JavaMacros)

file(GLOB_RECURSE sources src/*.java)

SET(JAVA_FLAGS "-Xlint")

ADD_JAR(dots_java "${sources}" ALL)

INSTALL(FILES dots_java.jar
  DESTINATION ${SAFIR_RUNTIME}/bin)
