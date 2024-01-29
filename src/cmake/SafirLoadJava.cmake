#
# Check for existence of Java compiler and set up compilation flags for Safir build tree.
#
# Defines Java_FOUND if a Java compiler and JNI stuff is found.
#

if ($ENV{SAFIR_DONT_BUILD_JAVA})
  message (STATUS "Not building java, since SAFIR_DONT_BUILD_JAVA is set")
  SET(Java_FOUND Java-NOTFOUND)
  return()
endif()

set(Java_FIND_QUIETLY True)
find_package(Java 11 COMPONENTS Development Runtime)
if (Java_Development_FOUND AND Java_Runtime_FOUND)
  if(${CMAKE_VERSION} VERSION_LESS "3.24")
    find_package(JNI QUIET)
  else()
    find_package(JNI QUIET OPTIONAL_COMPONENTS JVM)
  endif()
  if (JNI_FOUND)
    #Found everything!
    SET(Java_FOUND True)

    INCLUDE(UseJava)

    function(add_jar NAME)
        list(FIND ARGN "OUTPUT_DIR" has_output)
        if (NOT has_output EQUAL -1)
          message(FATAL_ERROR "add_jar of ${NAME} has OUTPUT_DIR set, which I kinda don't like")
        endif()

        _add_jar(${NAME} ${ARGN} OUTPUT_DIR ${CMAKE_JAVA_OUTPUT_DIRECTORY})
      endfunction()

    SET(CMAKE_JAVA_COMPILE_FLAGS -encoding UTF-8 -Xlint:unchecked -Xlint:deprecation --release 11)
  else()
    SET(Java_FOUND Java-NOTFOUND)
  endif()
  unset(Java_Development_FOUND)
  unset(Java_Runtime_FOUND)
  unset(JNI_FOUND)

else()
  SET(Java_FOUND Java-NOTFOUND)
endif()
if (NOT Java_FOUND)
  MESSAGE(STATUS "Failed to find the Java development tools, will not build Java interfaces")
endif()
