# - This is a support module for easy Mono/C# handling with CMake
# It defines the following macros:
#
# ADD_CS_LIBRARY (<target> <source> [ALL])
# ADD_CS_MODULE (<target> <source> [ALL])
# MERGE_CS_LIBRARY (<target> <source> [ALL])
# ADD_CS_EXECUTABLE (<target> <source> [ALL])
# ADD_CS_GUI_EXECUTABLE (<target> <source> [ALL]) 
# INSTALL_GAC (<target>)
# ADD_CS_RESOURCES(<resx-files> <prefix> <dir>)
#
# use ADD_CS_GUI_EXECUTABLE for gui apps (winforms etc) and ADD_CS_EXECUTABLE for console apps
# Note that the order of the arguments is important (including "ALL").
# It is recommended that you quote the arguments, especially <source>, if
# you have more than one source file.
#
# You can optionally set the variable CS_FLAGS to tell the macros whether
# to pass additional flags to the compiler. This is particularly useful to
# set assembly references, unsafe code, etc... These flags are always reset
# after the target was added so you don't have to care about that.
#
# The ADD_CS_RESOURCES is used to add resources (currently only applies
# to ADD_CS_EXECUTABLE). Call it before calling ADD_CS_EXECUTABLE to 
# get the resources built into the exe. 
# dir must be an absolute path, or a "."
#
# copyright (c) 2007 Arno Rehn arno@arnorehn.de
# copyright (c) 2008 Helio castro helio@kde.org
# copyright (c) 2009 Lars Hagstrom lars.hagstrom@saabgroup.com
# copyright (c) 2010 Mikael Wennerberg mikael.wennerberg@saabgroup.com
#
# Redistribution and use is allowed according to the terms of the GPL license.


# ----- support macros -----
MACRO(GET_CS_LIBRARY_TARGET_DIR)
	IF (NOT LIBRARY_OUTPUT_PATH)
		SET(CS_LIBRARY_TARGET_DIR ${CMAKE_CURRENT_BINARY_DIR})
	ELSE (NOT LIBRARY_OUTPUT_PATH)
		SET(CS_LIBRARY_TARGET_DIR ${LIBRARY_OUTPUT_PATH})
	ENDIF (NOT LIBRARY_OUTPUT_PATH)
ENDMACRO(GET_CS_LIBRARY_TARGET_DIR)

MACRO(GET_CS_EXECUTABLE_TARGET_DIR)
	IF (NOT EXECUTABLE_OUTPUT_PATH)
		SET(CS_EXECUTABLE_TARGET_DIR ${CMAKE_CURRENT_BINARY_DIR})
	ELSE (NOT EXECUTABLE_OUTPUT_PATH)
		SET(CS_EXECUTABLE_TARGET_DIR ${EXECUTABLE_OUTPUT_PATH})
	ENDIF (NOT EXECUTABLE_OUTPUT_PATH)
ENDMACRO(GET_CS_EXECUTABLE_TARGET_DIR)

MACRO(MAKE_PROPER_FILE_LIST source)
	FOREACH(file ${source})
		#We dont really do anything if we're on linux...
		IF (UNIX)
			SET(proper_file_list ${proper_file_list} ${file})
		ELSE()
			# first assume it's a relative path
			FILE(GLOB globbed ${CMAKE_CURRENT_SOURCE_DIR}/${file})
			IF(globbed)
				FILE(TO_NATIVE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/${file} native)
			ELSE(globbed)
				FILE(TO_NATIVE_PATH ${file} native)
			ENDIF(globbed)
			SET(proper_file_list ${proper_file_list} ${native})
			SET(native "")
		ENDIF()
	ENDFOREACH(file)
ENDMACRO(MAKE_PROPER_FILE_LIST)
# ----- end support macros -----

MACRO(ADD_CS_LIBRARY target source)
	GET_CS_LIBRARY_TARGET_DIR()

	#TODO: check versions to see if workaround is needed?
	IF(CSHARP_IS_MONO)
	    SET(CS_FLAGS ${CS_FLAGS} -define:FUNC_PTR_WORKAROUND)
	ENDIF()
	
	IF(CUSTOM_BUILD_TYPE STREQUAL "Debug")
	    SET(CS_FLAGS ${CS_FLAGS} -debug)
	ENDIF()

	SET(target_DLL "${CS_LIBRARY_TARGET_DIR}/${target}.dll")
	MAKE_PROPER_FILE_LIST("${source}")
	FILE(RELATIVE_PATH relative_path ${CMAKE_BINARY_DIR} ${target_DLL})
	
	SET (response_file ${CS_LIBRARY_TARGET_DIR}/command_line_${target}.rsp)
	FILE(REMOVE ${response_file})
	FOREACH(arg ${CS_FLAGS} -out:${target_DLL} -target:library ${proper_file_list})
		FILE(APPEND ${response_file} "\"${arg}\" ")
	ENDFOREACH()
	
	ADD_CUSTOM_COMMAND (OUTPUT ${target_DLL}
		COMMAND ${CSHARP_COMPILER} @${response_file}
		DEPENDS ${source}
		COMMENT "Building ${relative_path}")
	ADD_CUSTOM_TARGET (${target} ${ARGV2} DEPENDS ${target_DLL})
	SET(relative_path "")
	SET(proper_file_list "")
	SET(CS_FLAGS "")
ENDMACRO(ADD_CS_LIBRARY)

MACRO(ADD_CS_MODULE target source)
	GET_CS_LIBRARY_TARGET_DIR()

	#TODO: check versions to see if workaround is needed?
	IF(CSHARP_IS_MONO)
	    SET(CS_FLAGS ${CS_FLAGS} -define:FUNC_PTR_WORKAROUND)
	ENDIF()
	
	IF(CUSTOM_BUILD_TYPE STREQUAL "Debug")
	    SET(CS_FLAGS ${CS_FLAGS} -debug)
	ENDIF()

	SET(target_MODULE "${CS_LIBRARY_TARGET_DIR}/${target}.netmodule")
	MAKE_PROPER_FILE_LIST("${source}")
	FILE(RELATIVE_PATH relative_path ${CMAKE_BINARY_DIR} ${target_MODULE})
	
	SET (response_file ${CS_LIBRARY_TARGET_DIR}/command_line_${target}.rsp)
	FILE(REMOVE ${response_file})
	FOREACH(arg -out:${target_MODULE} ${CS_FLAGS} -target:module ${proper_file_list})
		FILE(APPEND ${response_file} "\"${arg}\" ")
	ENDFOREACH()
	
	ADD_CUSTOM_COMMAND (OUTPUT ${target_MODULE}
		COMMAND ${CSHARP_COMPILER} @${response_file}
		DEPENDS ${source}
		COMMENT "Building ${relative_path}")
	ADD_CUSTOM_TARGET (${target} ${ARGV2} DEPENDS ${target_MODULE})
	SET(relative_path "")
	SET(proper_file_list "")
	SET(CS_FLAGS "")
ENDMACRO(ADD_CS_MODULE)

MACRO(MERGE_CS_LIBRARY target source)
	GET_CS_LIBRARY_TARGET_DIR()

	IF(CUSTOM_BUILD_TYPE STREQUAL "Debug")
	    SET(CS_FLAGS ${CS_FLAGS} -debug)
	ENDIF()

	SET(target_DLL "${CS_LIBRARY_TARGET_DIR}/${target}.dll")
	MAKE_PROPER_FILE_LIST("${source}")
	FILE(RELATIVE_PATH relative_path ${CMAKE_BINARY_DIR} ${target_DLL})
	
	SET (response_file ${CS_LIBRARY_TARGET_DIR}/command_line_${target}.rsp)
	FILE(REMOVE ${response_file})
	FOREACH(arg ${CS_FLAGS} -out:${target_DLL} -target:library ${proper_file_list})
		FILE(APPEND ${response_file} "\"${arg}\" ")
	ENDFOREACH()
	
	ADD_CUSTOM_COMMAND (OUTPUT ${target_DLL}
		COMMAND ${CSHARP_LINKER} @${response_file}
		DEPENDS ${source}
		COMMENT "Building ${relative_path}")
	ADD_CUSTOM_TARGET (${target} ${ARGV2} DEPENDS ${target_DLL})
	SET(relative_path "")
	SET(proper_file_list "")
	SET(CS_FLAGS "")
ENDMACRO(MERGE_CS_LIBRARY)


MACRO(ADD_CS_EXECUTABLE target source)
	GET_CS_EXECUTABLE_TARGET_DIR()
	
	#TODO: check versions to see if workaround is needed?
	IF(CSHARP_IS_MONO)
	    SET(CS_FLAGS ${CS_FLAGS} -define:FUNC_PTR_WORKAROUND)
	ENDIF()
	
	# FIXME:
	# Seems like cmake doesn't like the ".exe" ending for custom commands.
	# If we call it ${target}.exe, 'make' will later complain about a missing rule.
	# We temporarily add ".csexe", and the user has to do a RENAME in the install step
	SET(target_EXE "${CS_EXECUTABLE_TARGET_DIR}/${target}.csexe")
	
	IF(CUSTOM_BUILD_TYPE STREQUAL "Debug")
	    SET(CS_FLAGS ${CS_FLAGS} -debug)
	ENDIF()	
	
	MAKE_PROPER_FILE_LIST("${source}")
	FILE(RELATIVE_PATH relative_path ${CMAKE_BINARY_DIR} ${target_EXE})
	
	SET (response_file ${CS_EXECUTABLE_TARGET_DIR}/cs_command_line.rsp)
	FILE(REMOVE ${response_file})
	FOREACH(arg ${CS_FLAGS} -out:${target_EXE} ${proper_file_list} ${resource_command_line})
		FILE(APPEND ${response_file} "\"${arg}\" ")
	ENDFOREACH()
	
	ADD_CUSTOM_COMMAND (OUTPUT "${target_EXE}"
		COMMAND ${CSHARP_COMPILER} @${response_file}
		DEPENDS ${source} ${resource_files}
		COMMENT "Building ${relative_path}")
	ADD_CUSTOM_TARGET ("${target}" "${ARGV2}" DEPENDS "${target_EXE}")
	SET(relative_path "")
	SET(proper_file_list "")
	SET(CS_FLAGS "")
    SET(resource_command_line "")
    SET(resource_files "")
ENDMACRO(ADD_CS_EXECUTABLE)

MACRO(ADD_CS_GUI_EXECUTABLE target source)
	GET_CS_EXECUTABLE_TARGET_DIR()
	
	#TODO: check versions to see if workaround is needed?
	IF(CSHARP_IS_MONO)
	    SET(CS_FLAGS ${CS_FLAGS} -define:FUNC_PTR_WORKAROUND)
	ENDIF()
	
	# FIXME:
	# Seems like cmake doesn't like the ".exe" ending for custom commands.
	# If we call it ${target}.exe, 'make' will later complain about a missing rule.
	# We temporarily add ".csexe", and the user has to do a RENAME in the install step
	SET(target_EXE "${CS_EXECUTABLE_TARGET_DIR}/${target}.csexe")
	
	IF(CUSTOM_BUILD_TYPE STREQUAL "Debug")
	    SET(CS_FLAGS ${CS_FLAGS} -debug)
	ENDIF()	
	
	MAKE_PROPER_FILE_LIST("${source}")
	FILE(RELATIVE_PATH relative_path ${CMAKE_BINARY_DIR} ${target_EXE})
	
	SET (response_file ${CS_EXECUTABLE_TARGET_DIR}/cs_command_line.rsp)
	FILE(REMOVE ${response_file})
	FOREACH(arg ${CS_FLAGS} -out:${target_EXE} -target:winexe ${proper_file_list} ${resource_command_line})
		FILE(APPEND ${response_file} "\"${arg}\" ")
	ENDFOREACH()
	
	ADD_CUSTOM_COMMAND (OUTPUT "${target_EXE}"
		COMMAND ${CSHARP_COMPILER} @${response_file}
		DEPENDS ${source} ${resource_files}
		COMMENT "Building ${relative_path}")
	ADD_CUSTOM_TARGET ("${target}" "${ARGV2}" DEPENDS "${target_EXE}")
	SET(relative_path "")
	SET(proper_file_list "")
	SET(CS_FLAGS "")
    SET(resource_command_line "")
    SET(resource_files "")
ENDMACRO(ADD_CS_GUI_EXECUTABLE)


MACRO(ADD_CS_RESOURCES resources resource_prefix working_directory)
  GET_CS_EXECUTABLE_TARGET_DIR()
  MAKE_PROPER_FILE_LIST("${resources}")
  SET(proper_resources ${proper_file_list})
  
  FOREACH(resx_file ${proper_resources})
    STRING(REGEX REPLACE "([\\/])([a-zA-Z.0-9]+).resx" "\\1${resource_prefix}.\\2.resources" resource_file ${resx_file})
    SET(resource_files ${resource_files} ${resource_file})
    SET(resource_command_line ${resource_command_line} -res:${resource_file})

    ADD_CUSTOM_COMMAND(OUTPUT ${resource_file}
      COMMAND ${RESGEN_EXECUTABLE} ARGS ${resx_file} ${resource_file}
      DEPENDS ${resx_file}
      WORKING_DIRECTORY ${working_directory}
      )
    
  ENDFOREACH(resx_file)

  SET(proper_file_list "")
  SET(proper_resources "")
  SET(resource_file "")
  SET(resx_file "")
ENDMACRO(ADD_CS_RESOURCES)

MACRO(INSTALL_CS_DEBUG_INFO)
  IF (CSHARP_IS_MONO)
    STRING(REPLACE .dll .dll${DEBUG_INFO_FILE_SUFFIX} location ${target_DLL} ${target_EXE})
#    STRING(REPLACE .exe .exe${DEBUG_INFO_FILE_SUFFIX} location ${location})
    STRING(REPLACE .csexe .csexe${DEBUG_INFO_FILE_SUFFIX} location ${location})
  ELSE()
    STRING(REPLACE .dll ${DEBUG_INFO_FILE_SUFFIX} location ${target_DLL} ${target_EXE})
#    STRING(REPLACE .exe ${DEBUG_INFO_FILE_SUFFIX} location ${location})
    STRING(REPLACE .csexe ${DEBUG_INFO_FILE_SUFFIX} location ${location})
  ENDIF()
  
  STRING(REGEX REPLACE ".*/" "" rename_to ${location})
  INSTALL(FILES ${location} 
    DESTINATION ${SAFIR_RUNTIME}/bin 
    CONFIGURATIONS Debug
    RENAME ${rename_to})
  set(location "")
  set(rename_to "")
ENDMACRO()

#MACRO(INSTALL_GAC target)
#	GET_CS_LIBRARY_TARGET_DIR()
#
#	IF(NOT WIN32)
#		INCLUDE(FindPkgConfig)
#		PKG_SEARCH_MODULE(MONO_CECIL mono-cecil)
#		if(MONO_CECIL_FOUND)
#			EXECUTE_PROCESS(COMMAND ${PKG_CONFIG_EXECUTABLE} mono-cecil --variable=assemblies_dir OUTPUT_VARIABLE GAC_ASSEMBLY_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)
#		endif(MONO_CECIL_FOUND)
#		
#		PKG_SEARCH_MODULE(CECIL cecil)
#		if(CECIL_FOUND)
#			EXECUTE_PROCESS(COMMAND ${PKG_CONFIG_EXECUTABLE} cecil --variable=assemblies_dir OUTPUT_VARIABLE GAC_ASSEMBLY_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)
#		endif(CECIL_FOUND)
#
#		if(CECIL_FOUND OR MONO_CECIL_FOUND)
#			INSTALL(CODE "EXECUTE_PROCESS(COMMAND ${GACUTIL_EXECUTABLE} -i ${CS_LIBRARY_TARGET_DIR}/${target}.dll -package 2.0 -root ${CMAKE_CURRENT_BINARY_DIR})")
#			MAKE_DIRECTORY(${CMAKE_CURRENT_BINARY_DIR}/mono/)
#			INSTALL(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mono/ DESTINATION ${GAC_ASSEMBLY_DIR} )
#		endif(CECIL_FOUND OR MONO_CECIL_FOUND)
#	ELSE(NOT WIN32)
#		INSTALL(CODE "EXECUTE_PROCESS(COMMAND ${GACUTIL_EXECUTABLE} -i ${CS_LIBRARY_TARGET_DIR}/${target}.dll -package 2.0)")
#	ENDIF(NOT WIN32)
#
#ENDMACRO(INSTALL_GAC target)
