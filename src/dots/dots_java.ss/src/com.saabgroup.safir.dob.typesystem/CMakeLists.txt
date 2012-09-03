cmake_minimum_required(VERSION 2.8)

project(dots_java Java)
find_package(Java REQUIRED)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)
INCLUDE(JavaMacros)

file(GLOB_RECURSE sources src/*.java)

ADD_JAR(dots_java "${sources}" ALL)

INSTALL(FILES dots_java.jar
  DESTINATION ${SAFIR_RUNTIME}/bin)
