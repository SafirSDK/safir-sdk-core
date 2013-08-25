find_package(Java REQUIRED)

INCLUDE(JavaMacros)

file(GLOB_RECURSE sources src/*.java)

SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/douf_java.jar)

ADD_JAR(douf_test_java "${sources}" ALL)

if (UNIX)
  set(PATH_SEPARATOR ":")
else()
  set(PATH_SEPARATOR ";")
endif()

ADD_TEST(TimeProvider_java_test ${Java_JAVA_EXECUTABLE} -cp "${SAFIR_RUNTIME}/bin/douf_java.jar${PATH_SEPARATOR}douf_test_java.jar" Test)
SET_TESTS_PROPERTIES(TimeProvider_java_test PROPERTIES ENVIRONMENT "SAFIR_TEST_CONFIG_OVERRIDE=${SOURCE_ROOT}/src/tests/test_support/test_config")
