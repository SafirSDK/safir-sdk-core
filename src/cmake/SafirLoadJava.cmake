#
# Check for existence of Java compiler and set up compilation flags for Safir build tree.
#
# Defines Java_FOUND if a Java compiler and JNI stuff is found.
#
# Also defines a function SAFIR_JAVAH which is described below.
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

    #old cmake versions have a UseJava file that does not support manifest files
    #so we instead use one that is a copy of the version from cmake 3...
    if (CMAKE_VERSION VERSION_LESS "3.0.0")
      INCLUDE(UseJavaWithManifest/UseJava)
    else()
      INCLUDE(UseJava)
    endif()

    SET(CMAKE_JAVA_COMPILE_FLAGS -encoding UTF-8 -Xlint:unchecked)
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

#
# Generate a C/C++ header file for a Java Native Interface (JNI) class.
#
# Usage: SAFIR_JAVAH(JAR <jar name>
#                    CLASS <jni class>
#                    OUTPUT_FILES <expected output files>
#                    OUTPUT_DIR <directory where output should be put>)
#
# A target <jar name>_javah will be defined, which will generate the needed files.
# Set up a dependency from your shared library to this target to ensure the
# files get generated correctly.
#
function (SAFIR_JAVAH)
  cmake_parse_arguments(_javah "" "JAR;CLASS;OUTPUT_DIR" "OUTPUT_FILES" ${ARGN})

  if (NOT _javah_JAR)
    message(FATAL_ERROR "SAFIR_JAVAH: JAR not specified")
  endif()

  if (NOT _javah_CLASS)
    message(FATAL_ERROR "SAFIR_JAVAH: CLASS not specified")
  endif()

  if (NOT _javah_OUTPUT_FILES)
    message(FATAL_ERROR "SAFIR_JAVAH: OUTPUT_FILES not specified")
  endif()

  if (NOT _javah_OUTPUT_DIR)
    message(FATAL_ERROR "SAFIR_JAVAH: OUTPUT_DIR not specified")
  endif()

  if (NOT "${_javah_UNPARSED_ARGUMENTS}" STREQUAL "")
    message(FATAL_ERROR "Unknown argument to SAFIR_JAVAH '${_javah_UNPARSED_ARGUMENTS}'")
  endif()

  foreach(file ${_javah_OUTPUT_FILES})
    list(APPEND _javah_outputs ${_javah_OUTPUT_DIR}/${file})
  endforeach()

  get_target_property(_javah_jar_file ${_javah_JAR} JAR_FILE)

  ADD_CUSTOM_COMMAND(OUTPUT
    ${_javah_outputs}

    COMMAND ${Java_JAVAH_EXECUTABLE} -classpath $<TARGET_PROPERTY:${_javah_JAR},JAR_FILE> ${_javah_CLASS}

    DEPENDS ${_javah_JAR} ${_javah_jar_file}
    WORKING_DIRECTORY ${_javah_OUTPUT_DIR}

    COMMENT "Generating javah header files from ${_javah_JAR}")

  ADD_CUSTOM_TARGET(${_javah_JAR}_javah DEPENDS
    ${_javah_outputs})
endfunction()
