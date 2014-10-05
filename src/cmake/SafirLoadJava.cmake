# We need to find both Java and JNI stuff, hence the convoluted mess below.
# Anyway, if Java_FOUND is set we're ok!
#
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
      INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/cmake/UseJavaWithManifest/UseJava.cmake)
    else()
      INCLUDE(UseJava)
    endif()

	SET(CMAKE_JAVA_COMPILE_FLAGS -encoding UTF-8 -source 1.6)
  else()
    SET(Java_FOUND Java-NOTFOUND)
  endif()
  unset(Java_Development_FOUND)  
  unset(Java_Runtime_FOUND)
else()
  SET(Java_FOUND Java-NOTFOUND)
endif()
if (NOT Java_FOUND)
  MESSAGE(WARNING "Failed to find the Java development tools, will not build Java interfaces")
endif()

#TODO: javadoc?

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
    
    DEPENDS ${_javah_jar_file}
    WORKING_DIRECTORY ${_javah_OUTPUT_DIR}
    
    COMMENT "Generating javah header files from ${_javah_JAR}")
  
  ADD_CUSTOM_TARGET(${_javah_JAR}_javah DEPENDS
    ${_javah_outputs})
endfunction()
