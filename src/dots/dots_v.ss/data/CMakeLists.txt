cmake_minimum_required(VERSION 2.6)

PROJECT (dots_generated)

#we need to set up include dirs before including cmake_settings.txt
#since otherwise sdk/include will be before cpp/include in the include path.
INCLUDE_DIRECTORIES(. cpp/include)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

if (NOT NO_GENERATE_CODE)
    MESSAGE("-- Generating code from dou and dom files...")
	#this will cause dots_v to be run. In the gen directory
	#there should be a CMakeLists.txt file that runs dots_v.
	#This needs to be a try_compile target to allow dots_v
	#to generate "unpredictable" files.
	try_compile (result 
	  ${dots_generated_SOURCE_DIR}/gen 
	  ${dots_generated_SOURCE_DIR}/gen 
	  dots_generated-code
	  CMAKE_FLAGS -DCMAKE_BUILD_TYPE:string=Release
	  OUTPUT_VARIABLE res_txt)

	if(NOT result)
	  MESSAGE(FATAL_ERROR "Error(s) when generating code:\n${res_txt}")
	endif()
    MESSAGE("-- Code generation completed.")
endif()

add_custom_command(OUTPUT tags cpp dotnet java ada
  COMMAND echo "This line is intentionally left blank")

add_custom_target(code ALL
  DEPENDS tags)
  
#we want to ensure that a "make" will cause cmake . to be rerun
# so we touch the CMakeLists.txt file to force a regenerate...
add_custom_target(ensure_rebuild ALL
  COMMAND ${CMAKE_COMMAND} -E touch CMakeLists.txt)


# --- C++ --- #
if (NOT NO_CXX)
	FILE(GLOB cpp_files cpp/*.cpp)
	FILE(GLOB_RECURSE h_files cpp/*.h)
	ADD_LIBRARY(dots_generated-cpp SHARED ${cpp_files} ${h_files})

	ADD_DEFINITIONS(-DDOTS_GENERATED_CPP_EXPORTS)

	if (NOT MSVC)
	  TARGET_LINK_LIBRARIES(dots_generated-cpp dots_cpp)
	endif()

	INSTALL(DIRECTORY cpp/include DESTINATION ${SAFIR_SDK}
			PATTERN ".svn" EXCLUDE
			PATTERN "*~" EXCLUDE)

	INSTALL(TARGETS dots_generated-cpp
			RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
			LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
			ARCHIVE DESTINATION ${SAFIR_SDK}/lib)
	
	INSTALL_DEBUG_INFO(dots_generated-cpp)
endif()

# --- C# --- #
if (NOT NO_DOTNET)
	find_package(CSharp REQUIRED)
	INCLUDE(CSharpMacros)

	FILE(GLOB cs_files dotnet/*.cs)
	FILE(GLOB cs_meta_files *.cs)

	SET(CS_FLAGS ${COMMON_CS_FLAGS} "-keyfile:${dots_generated_SOURCE_DIR}/dots_generated-dotnet.snk"
									"-r:${SAFIR_RUNTIME}/bin/Safir.Dob.Typesystem.dll")
	ADD_CS_LIBRARY(dots_generated-dotnet "${cs_meta_files};${cs_files}" ALL)


	INSTALL(FILES ${target_DLL} #This is the C# dll
	  DESTINATION ${SAFIR_RUNTIME}/bin)
	  
	INSTALL_CS_DEBUG_INFO()
endif()

# --- Java --- #
if (NOT NO_JAVA)
	find_package(Java REQUIRED)
	INCLUDE(JavaMacros)
	file(GLOB_RECURSE java_files java/src/*.java)

	SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/dots_java.jar)

	SET(JAVA_MANIFEST ${dots_generated_SOURCE_DIR}/Manifest.java.txt)
	
	ADD_JAR(dots_generated-java "${java_files}" ALL)
	INSTALL(FILES ${target_JAR}
	  DESTINATION ${SAFIR_RUNTIME}/bin)
endif()

# --- Ada --- #
if (NOT NO_ADA)
	FILE(MAKE_DIRECTORY ada/lib ada/obj ada/ali ada/interface )
        
    # Update the Library_Interface list in dots_generated_ada_library.gpr
    #we take the first item out of the list, and put it last in the resulting
    #variable, without the ending comma character.
    FILE(GLOB ads_units RELATIVE ${dots_generated_SOURCE_DIR}/ada ada/*.ads)
    LIST(GET ads_units 0 unit)
    LIST(REMOVE_AT ads_units 0)
    STRING(REPLACE ".ads" "" unit ${unit})
    STRING(REPLACE "-" "." unit ${unit})
    SET(interface_units "\n\t\"${unit}\"")
    FOREACH(unit ${ads_units})
       STRING(REPLACE ".ads" "" unit ${unit})
       STRING(REPLACE "-" "." unit ${unit})
	   SET(interface_units "${interface_units},\n\t\"${unit}\"")
	ENDFOREACH()    
    CONFIGURE_FILE (${dots_generated_SOURCE_DIR}/dots_generated_ada_library.gpr_template ${dots_generated_SOURCE_DIR}/dots_generated_ada_library.gpr)

	SET(gnatmake_arguments -XBUILD=${CUSTOM_BUILD_TYPE})
	ADD_CUSTOM_TARGET(dots_generated_ada ALL
					  COMMAND gnatmake ${gnatmake_arguments} -P ${dots_generated_SOURCE_DIR}/dots_generated_ada_library.gpr
                      WORKING_DIRECTORY ${dots_generated_SOURCE_DIR}
					  COMMENT "Building dots_generated_ada using gnatmake"
					  VERBATIM)
        	
	INSTALL(DIRECTORY ada/interface/
			DESTINATION ${SAFIR_SDK}/ada/dots_generated_ada/interface)
	INSTALL(DIRECTORY ada/ali/
			DESTINATION  ${SAFIR_SDK}/ada/dots_generated_ada/ali)
	INSTALL(FILES dots_generated_ada.gpr
			DESTINATION ${SAFIR_SDK}/ada)
	if (WIN32)
	   INSTALL(FILES ada/lib/libdots_generated_ada.dll
			DESTINATION ${SAFIR_RUNTIME}/bin)
	elseif (UNIX)
	   INSTALL(FILES ada/lib/libdots_generated_ada.so
			   DESTINATION ${SAFIR_RUNTIME}/lib/)
	endif(WIN32)
endif()

# --- Installation rules for dou and dom files--- #
if (NOT NO_DOU_INSTALL)
	INSTALL(CODE "FILE(REMOVE_RECURSE \"${SAFIR_RUNTIME}/data/text/dots/classes/\")")

	FILE(GLOB dom_files *.dom)
	FILE(GLOB dou_files *.dou)
	FILE(GLOB namespace_files *.namespace.txt)

	INSTALL(FILES ${dou_files} ${dom_files} ${namespace_files}
			DESTINATION ${SAFIR_RUNTIME}/data/text/dots/classes/)
endif()

