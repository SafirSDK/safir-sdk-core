cmake_minimum_required(VERSION 2.8)

project(dose_test_util_java Java)
find_package(Java REQUIRED)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)
INCLUDE(JavaMacros)

file(GLOB_RECURSE sources src/*.java)

ADD_JAR(dose_test_util_java "${sources}" ALL)

INSTALL(FILES dose_test_util_java.jar
  DESTINATION ${SAFIR_RUNTIME}/bin)
