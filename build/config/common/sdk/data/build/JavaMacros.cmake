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
	
	IF(CUSTOM_BUILD_TYPE STREQUAL "Debug")
	    SET(JAVA_FLAGS ${JAVA_FLAGS} -g)
	ENDIF()

	SET(target_JAR "${JAR_TARGET_DIR}/${target}.jar")
	FILE(RELATIVE_PATH relative_path ${CMAKE_BINARY_DIR} ${target_JAR})
	
	SET (response_file "${JAR_TARGET_DIR}/javac_command_line.rsp")
	FILE(RELATIVE_PATH relative_response_file ${CMAKE_CURRENT_BINARY_DIR} ${response_file})
	FILE(REMOVE ${response_file})
	if(JAVA_CLASSPATH)
		FILE(WRITE ${response_file} "-cp \"${JAVA_CLASSPATH}\" ")
	endif()
    FILE(APPEND ${response_file} "-encoding UTF-8")
	FOREACH(arg ${JAVA_FLAGS} -d ${JAR_TARGET_DIR}/bin ${source})
		FILE(APPEND ${response_file} "\"${arg}\" ")
	ENDFOREACH()
	
	if (JAVA_MANIFEST)
		set (command_start cmf ${JAVA_MANIFEST})
	else()
		set (command_start cf)
	endif()
	
	ADD_CUSTOM_COMMAND (OUTPUT ${target_JAR} ${JAR_TARGET_DIR}/bin
		COMMAND ${CMAKE_COMMAND} -E make_directory ${JAR_TARGET_DIR}/bin
		COMMAND ${JAVA_COMPILE} @${relative_response_file} -J-Xms256m -J-Xmx256m 
		COMMAND ${JAVA_ARCHIVE} ${command_start} ${target_JAR} -C "${JAR_TARGET_DIR}/bin"  .
		DEPENDS ${source} ${JAVA_MANIFEST}
		COMMENT "Building ${relative_path}")
	ADD_CUSTOM_TARGET (${target} ${ARGV2} DEPENDS ${target_JAR})
	SET(command_start "")
	SET(relative_path "")
	SET(JAVA_FLAGS "")
	SET(JAVA_MANIFEST "")
ENDMACRO()


