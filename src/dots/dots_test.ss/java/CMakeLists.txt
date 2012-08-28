if (NOT SAFIR_JAVA_SUPPORT)
     ADD_CUSTOM_TARGET(empty_dots_test_java ALL
                       COMMENT "SAFIR_JAVA_SUPPORT set to false, dots_test_java will not be built.")
     INSTALL(CODE "MESSAGE(\"SAFIR_JAVA_SUPPORT set to false, dots_test_java will not be installed.\")")
     return()
endif (NOT SAFIR_JAVA_SUPPORT)

find_package(Java REQUIRED)

INCLUDE(JavaMacros)

file(GLOB_RECURSE sources src/*.java)

SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/dots_java.jar ${SAFIR_RUNTIME}/bin/dots_generated-java.jar)

SET(JAVA_MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/Manifest.txt)

ADD_JAR(dots_test_java "${sources}" ALL)


