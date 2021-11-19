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
find_package(Java COMPONENTS Development Runtime)
if (Java_Development_FOUND AND Java_Runtime_FOUND)
  find_package(JNI QUIET)
  if (JNI_FOUND)
    #Found everything!
    SET(Java_FOUND True)

    INCLUDE(UseJava)

    SET(CMAKE_JAVA_COMPILE_FLAGS -encoding UTF-8 -Xlint:unchecked -Xlint:deprecation)
  else()
    SET(Java_FOUND Java-NOTFOUND)
  endif()
  unset(Java_Development_FOUND)
  unset(Java_Runtime_FOUND)

else()
  SET(Java_FOUND Java-NOTFOUND)
endif()
if (NOT Java_FOUND)
  MESSAGE(STATUS "Failed to find the Java development tools, will not build Java interfaces")
endif()
