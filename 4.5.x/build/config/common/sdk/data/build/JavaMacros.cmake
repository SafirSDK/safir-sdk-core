# - This is a support module for easy Java handling with CMake
# It defines the following macros:
#
# ADD_JAR (<target> <source> [ALL])
#
# Note that the order of the arguments is important (including "ALL").
# It is recommended that you quote the arguments, especially <source>, if
# you have more than one source file.
#
# You can optionally set the variable JAVA_FLAGS to tell the macros whether
# to pass additional flags to the compiler. This is particularly useful to
# xlint settings, etc... These flags are always reset
# after the target was added so you don't have to care about that.
#
# copyright (c) 2007 Arno Rehn arno@arnorehn.de
# copyright (c) 2008 Helio castro helio@kde.org
# copyright (c) 2009 Lars Hagstrom lars.hagstrom@saabgroup.com
# copyright (c) 2011 Lars Hagstrom lars@foldspace.nu
#
# Redistribution and use is allowed according to the terms of the GPL license.


# ----- support macros -----
MACRO(GET_JAR_TARGET_DIR)
    IF (NOT RUNTIME_OUTPUT_DIRECTORY)
        SET(JAR_TARGET_DIR ${CMAKE_CURRENT_BINARY_DIR})
    ELSE ()
        SET(JAR_TARGET_DIR ${RUNTIME_OUTPUT_DIRECTORY})
    ENDIF ()
ENDMACRO()

MACRO(GET_JAVA_TARGET_BIN_DIR)
    IF (NOT JAR_TARGET_DIR_PREFIX)
        SET(JAVA_TARGET_BIN_DIR "${JAR_TARGET_DIR}/bin")
    ELSE ()
        SET(JAVA_TARGET_BIN_DIR "${JAR_TARGET_DIR}/${JAR_TARGET_DIR_PREFIX}_bin")
    ENDIF ()
ENDMACRO()

MACRO(SET_JAR_TARGET_DIR_PREFIX arg)
      SET(JAR_TARGET_DIR_PREFIX ${arg})
ENDMACRO()

#Some stuff for setting up a java classpath
if (UNIX)
  SET(classpath_separator ":")
else ()
  SET(classpath_separator "\;")
endif()

# ----- end support macros -----
MACRO(SET_JAVA_CLASSPATH)
  foreach(arg ${ARGV})
    if (NOT JAVA_CLASSPATH)
      SET(JAVA_CLASSPATH ${arg})
    else()
      SET(JAVA_CLASSPATH ${JAVA_CLASSPATH}${classpath_separator}${arg})
    endif()
  endforeach(arg ${ARGV})
ENDMACRO()


MACRO(ADD_JAR target source)
    GET_JAR_TARGET_DIR()
    GET_JAVA_TARGET_BIN_DIR()
    
    IF(CUSTOM_BUILD_TYPE STREQUAL "Debug")
        SET(JAVA_FLAGS ${JAVA_FLAGS} -g)
    ENDIF()
    SET(JAVA_FLAGS ${JAVA_FLAGS} -Xlint:all,-path)

    SET(target_JAR "${JAR_TARGET_DIR}/${target}.jar")
    FILE(RELATIVE_PATH relative_path ${CMAKE_BINARY_DIR} ${target_JAR})
    
    SET (response_file "${JAR_TARGET_DIR}/javac_${target}.rsp")
    FILE(RELATIVE_PATH relative_response_file ${CMAKE_CURRENT_BINARY_DIR} ${response_file})
    FILE(REMOVE ${response_file})
    if(JAVA_CLASSPATH)
        FILE(WRITE ${response_file} "-cp \"${JAVA_CLASSPATH}\" ")
    endif()
        FILE(APPEND ${response_file} "-encoding UTF-8")
    FOREACH(arg ${JAVA_FLAGS} -d ${JAVA_TARGET_BIN_DIR} ${source})
        FILE(APPEND ${response_file} "\"${arg}\" ")
    ENDFOREACH()

    if (NOT $ENV{VERBOSE} STREQUAL "")
      FILE(READ ${response_file} response_file_contents)
      MESSAGE("Contents of ${response_file} is ${response_file_contents}")
    endif()
    

    if (JAVA_MANIFEST)
        set (command_start cmf ${JAVA_MANIFEST})
    else()
        set (command_start cf)
    endif()
    
    ADD_CUSTOM_COMMAND (OUTPUT ${target_JAR} ${JAVA_TARGET_BIN_DIR}
        COMMAND ${CMAKE_COMMAND} -E make_directory ${JAVA_TARGET_BIN_DIR}
        COMMAND ${JAVA_COMPILE} @${relative_response_file} -J-Xms256m -J-Xmx256m 
        COMMAND ${JAVA_ARCHIVE} ${command_start} ${target_JAR} -C ${JAVA_TARGET_BIN_DIR}  .
        DEPENDS ${source} ${JAVA_MANIFEST}
        COMMENT "Building ${relative_path}")
    ADD_CUSTOM_TARGET (${target} ${ARGV2} DEPENDS ${target_JAR})
    SET_TARGET_PROPERTIES (${target} PROPERTIES LOCATION ${target_JAR})
    SET(command_start "")
    SET(relative_path "")
    SET(JAVA_FLAGS "")
    SET(JAVA_MANIFEST "")
ENDMACRO()


MACRO(CREATE_JAR target source)
    GET_JAR_TARGET_DIR()
    GET_JAVA_TARGET_BIN_DIR()
    
    SET(target_JAR "${JAR_TARGET_DIR}/${target}.jar")
    FILE(RELATIVE_PATH relative_path ${CMAKE_BINARY_DIR} ${target_JAR})
    file(MAKE_DIRECTORY ${JAVA_TARGET_BIN_DIR})

    if (JAVA_MANIFEST)
        set (command_start cmf ${JAVA_MANIFEST})
    else()
        set (command_start cf)
    endif()

    set (command_update uf)

    SET (response_file "${JAR_TARGET_DIR}/javac_${target}.rsp")
    FILE(RELATIVE_PATH relative_response_file ${CMAKE_CURRENT_BINARY_DIR} ${response_file})
    FILE(REMOVE ${response_file})

    list(APPEND tmp_list ${source})
    list(GET tmp_list 0 FIRST_SOURCE)
    list(REMOVE_AT tmp_list 0)

    ADD_CUSTOM_COMMAND (OUTPUT ${target_JAR}
        COMMAND ${JAVA_ARCHIVE} ${command_start} ${target_JAR} -C "${FIRST_SOURCE}" .
        DEPENDS ${source} ${JAVA_MANIFEST}
        COMMENT "Making jar ${relative_path}")
    ADD_CUSTOM_TARGET (${target} ${ARGV2} DEPENDS ${target_JAR})

    set(i 0)
    FOREACH(arg ${tmp_list})
      set(j ${i})
      math(EXPR i "${i} + 1")
      ADD_CUSTOM_COMMAND (OUTPUT ${target_JAR}_${i}
        COMMAND ${JAVA_ARCHIVE} ${command_update} ${target_JAR} -C "${arg}" .
        DEPENDS ${source} ${arg}
        COMMENT "Updating jar ${relative_path}")
      ADD_CUSTOM_TARGET (${target}_${i} ${ARGV2} DEPENDS ${target_JAR}_${i})
      add_dependencies(${target}_${i} ${target})
      if(NOT j EQUAL 0)
        add_dependencies(${target}_${i} ${target}_${j})
      endif()
    ENDFOREACH()   

    SET(relative_path "")
    SET(JAVA_FLAGS "")
    SET(JAVA_MANIFEST "")
ENDMACRO()

