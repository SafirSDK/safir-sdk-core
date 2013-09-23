cmake_minimum_required(VERSION 2.8)

if (NOT NO_JAVA)
  set(PROJ_JAVA Java)
endif()

PROJECT (dots_generated CXX ${PROJ_JAVA})

#we need to set up include dirs before including safir_sdk_core_config.cmake
#since otherwise sdk/include will be before cpp/include in the include path.
INCLUDE_DIRECTORIES(. cpp/include)

INCLUDE($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)
INCLUDE(cmake_depend.txt)
INCLUDE(PrecompiledHeader)

#just use the REBUILD variable, to avoid warnings.
SET (DUMMY ${REBUILD})

#################################
#            C++                #
#################################
if (NOT NO_CXX)
  FILE(GLOB_RECURSE h_files cpp/*.h)
  ##
  #Build one library for each namespace
  ##
  FOREACH(prefix ${DOTS_NS})

    FILE(GLOB cpp_files cpp/${prefix}*.cpp)

    #We need to copy the precomp header so that multiple libraries do not share the same pch/gch file
    configure_file(precompiled_header_for_cpp.h precompiled_header_${prefix}.h COPYONLY)

    if (MSVC)
      #Visual C++ needs a "special" cpp file in its source list for precompiled headers to work,
      #and also wants an absolute path to the header
      configure_file(precompiled_header_for_cpp.cpp precompiled_header_${prefix}.cpp)
      set(cpp_files ${cpp_files} precompiled_header_${prefix}.cpp)
      set(precompiled_header ${CMAKE_CURRENT_BINARY_DIR}/precompiled_header_${prefix}.h)
    else()
      #gcc wants a relative path to the header-
      file(RELATIVE_PATH precompiled_header ${CMAKE_CURRENT_SOURCE_DIR} 
        ${CMAKE_CURRENT_BINARY_DIR}/precompiled_header_${prefix}.h)
    endif()

    ADD_LIBRARY(dots_generated-${prefix}-cpp SHARED ${cpp_files} ${h_files})

    ADD_PRECOMPILED_HEADER(dots_generated-${prefix}-cpp 
                           ${precompiled_header}
                           FORCEINCLUDE)

    # check dependecies and build order
    if(DEFINED DOTS_NS_${prefix})
      SET(DOTS_TMP_NS DOTS_NS_${prefix})
      FOREACH(DOTS_DEPEND ${${DOTS_TMP_NS}})
        add_dependencies(dots_generated-${prefix}-cpp dots_generated-${DOTS_DEPEND}-cpp)
        TARGET_LINK_LIBRARIES(dots_generated-${prefix}-cpp dots_generated-${DOTS_DEPEND}-cpp)
      ENDFOREACH()
    endif()

    ADD_DEFINITIONS(-DDOTS_GENERATED_CPP_EXPORTS)


    if (MSVC)
      TARGET_LINK_LIBRARIES(dots_generated-${prefix}-cpp dots_generated-cpp)    
    else()
      TARGET_LINK_LIBRARIES(dots_generated-${prefix}-cpp dots_cpp)
    endif()

    INSTALL(TARGETS dots_generated-${prefix}-cpp
      RUNTIME DESTINATION ${SAFIR_RUNTIME}/bin
      LIBRARY DESTINATION ${SAFIR_RUNTIME}/lib
      ARCHIVE DESTINATION ${SAFIR_SDK}/lib)
    
    INSTALL_DEBUG_INFO(dots_generated-${prefix}-cpp)
  ENDFOREACH()

  ##
  # build main dots_generated-cpp
  ##
  ADD_LIBRARY(dots_generated-cpp SHARED dll_imports.cpp dll_imports.h)  
  ADD_DEFINITIONS(-DDOTS_GENERATED_CPP_EXPORTS)

  if (NOT MSVC)
    TARGET_LINK_LIBRARIES(dots_generated-cpp dots_cpp dl)
    
    FOREACH(prefix ${DOTS_NS})
      TARGET_LINK_LIBRARIES(dots_generated-cpp dots_generated-${prefix}-cpp)
    ENDFOREACH()

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

#################################
#          DOTNET               #
#################################
if (NOT NO_DOTNET)
  find_package(CSharp REQUIRED)
  INCLUDE(CSharpMacros)

  GET_CS_LIBRARY_TARGET_DIR()

  FILE(GLOB cs_files dotnet/*.cs)
  FILE(GLOB cs_meta_files *.cs)

  SET(CS_FLAGS ${COMMON_CS_FLAGS} "-keyfile:${dots_generated_SOURCE_DIR}/dots_generated-dotnet.snk"
    "-r:${SAFIR_RUNTIME}/bin/Safir.Dob.Typesystem.dll")
  ADD_CS_LIBRARY(dots_generated-dotnet "${cs_meta_files};${cs_files}" ALL)


  INSTALL(FILES ${target_DLL} #This is the C# dll
    DESTINATION ${SAFIR_RUNTIME}/bin)
  
  INSTALL_CS_DEBUG_INFO()
endif()

#################################
#          JAVA                 #
#################################
if (NOT NO_JAVA)

  find_package(Java REQUIRED)
  INCLUDE(JavaMacros)
  GET_JAR_TARGET_DIR()

  ##
  #Build one library for each namespace
  ##
  FOREACH(prefix ${DOTS_NS})
    set(JAVA_CLASSPATH "")
    SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/dots_java.jar)
    SET(JAVA_MANIFEST ${dots_generated_SOURCE_DIR}/Manifest.java.txt)

    ##
    #check manifests for namespace
    ##
    set(NS_FOUND "")
    set(TMP_NS "")
    set(java_files "")

    string(TOLOWER ${prefix} ns_prefix)
    
    file(GLOB_RECURSE java_files java/src/${ns_prefix}/*.java)
    
    # find all namespaces
    file(GLOB_RECURSE TMP_NS ${dots_generated_SOURCE_DIR}/${prefix}*-java.namespace.txt)
    list(LENGTH TMP_NS TMP_NS_LENGTH)
    if (TMP_NS_LENGTH GREATER 0)
      FOREACH(NS ${TMP_NS})
        set(JAVA_NS "")
        set(tmp_files "")
        
        file(STRINGS ${NS} TMP_STRINGS)
        FOREACH(line ${TMP_STRINGS})
          string(SUBSTRING ${line} 0 1 first)
          if (NOT ${first} STREQUAL "#")
            set(JAVA_NS ${line})
            string(STRIP ${JAVA_NS} JAVA_NS)
          endif() 
        ENDFOREACH()

        if (JAVA_NS)
          get_filename_component(message_prefix ${NS} NAME)
          string(REPLACE "-java.namespace.txt" "" message_prefix ${message_prefix})
          #message("JAVA: Using ${JAVA_NS} for ${message_prefix}")
          STRING(REPLACE "." "/" JAVA_NS ${JAVA_NS})       
          file(GLOB_RECURSE tmp_files java/src/${JAVA_NS}/${ns_prefix}/*.java)
          list(APPEND java_files "${tmp_files}")
          set(NS_FOUND TRUE)
        endif()

      ENDFOREACH()            
    endif()

    #dependencies
    if(DEFINED DOTS_NS_${prefix})
      SET(DOTS_TMP_NS DOTS_NS_${prefix})
      FOREACH(DOTS_DEPEND ${${DOTS_TMP_NS}})
        SET_JAVA_CLASSPATH(dots_generated-${DOTS_DEPEND}-java.jar)
      ENDFOREACH()
    endif()
    
    SET_JAR_TARGET_DIR_PREFIX(dots_generated-${prefix}-java)
    ADD_JAR(dots_generated-${prefix}-java "${java_files}" ALL)

    #cmake dependencies - build order
    if(DEFINED DOTS_NS_${prefix})
      SET(DOTS_TMP_NS DOTS_NS_${prefix})
      FOREACH(DOTS_DEPEND ${${DOTS_TMP_NS}})
        add_dependencies(dots_generated-${prefix}-java dots_generated-${DOTS_DEPEND}-java)
      ENDFOREACH()
    endif()

  ENDFOREACH()

  ##
  # Make main dots_generated-java
  ##
  SET_JAVA_CLASSPATH(${SAFIR_RUNTIME}/bin/dots_java.jar)
  SET(JAVA_MANIFEST ${dots_generated_SOURCE_DIR}/Manifest.java.txt)

  FOREACH(prefix ${DOTS_NS})
    list(APPEND java_dirs "${CMAKE_CURRENT_BINARY_DIR}/dots_generated-${prefix}-java_bin")
  ENDFOREACH()

  GET_JAR_TARGET_DIR()

  SET_JAR_TARGET_DIR_PREFIX(dots_generated-java)

  CREATE_JAR(dots_generated-java "${java_dirs}" ALL)

  FOREACH(prefix ${DOTS_NS})
    add_dependencies(dots_generated-java dots_generated-${prefix}-java)
  ENDFOREACH()

  INSTALL(FILES ${target_JAR}
    DESTINATION ${SAFIR_RUNTIME}/bin)

endif()


#################################
#        ADA                    #
#################################
if (NOT NO_ADA)

  # Change camelcase to ADA style
  function(string_to_ada_style IN_STR OUT_STR)
    string(REGEX REPLACE "([a-z])([A-Z]+)" "\\1_\\2" TMP_STR "${IN_STR}")
    string(REGEX REPLACE "([A-Z]|[a-z]|[0-9])([A-Z])([a-z]+)" "\\1_\\2\\3" TMP_STR "${TMP_STR}")
    string(REGEX REPLACE "([A-Z]|[a-z])([0-9]+)" "\\1_\\2" TMP_STR "${TMP_STR}")
    string(REGEX REPLACE "([0-9]+)([A-Z]|[a-z])" "\\1_\\2" TMP_STR "${TMP_STR}")
    string(TOLOWER ${TMP_STR} TMP_STR)
    set(${OUT_STR} "${TMP_STR}" PARENT_SCOPE)
  endfunction(string_to_ada_style)

  
  ##
  #Build one library for each namespace
  ##
  FOREACH(prefix ${DOTS_NS})
    #message("Build ADA ${prefix}")

    string_to_ada_style(${prefix} CC_prefix)
    #message("${prefix} converted to ADA style ${CC_prefix}.")

    SET(library_name "${CC_prefix}")

    FILE(MAKE_DIRECTORY ada/${CC_prefix})
    FILE(MAKE_DIRECTORY ada/${CC_prefix}/obj ada/${CC_prefix}/lib ada/${CC_prefix}/ali ada/${CC_prefix}/interface )


    FILE(GLOB ads_units RELATIVE ${dots_generated_SOURCE_DIR}/ada ada/${CC_prefix}*.ads)
    FILE(GLOB adb_units RELATIVE ${dots_generated_SOURCE_DIR}/ada ada/${CC_prefix}*.adb)

    # Update the Library_Interface list in dots_generated_ada_library.gpr
    # we take the first item out of the list, and put it last in the resulting
    # variable, without the ending comma character.
    LIST(GET ads_units 0 unit)
    LIST(REMOVE_AT ads_units 0)
    SET(source_files "\n\t\"${unit}\"")
    STRING(REPLACE ".ads" "" unit ${unit})
    STRING(REPLACE "-" "." unit ${unit})
    SET(interface_units "\n\t\"${unit}\"")
    FOREACH(unit ${ads_units})
      SET(source_files "${source_files},\n\t\"${unit}\"")
      STRING(REPLACE ".ads" "" unit ${unit})
      STRING(REPLACE "-" "." unit ${unit})
      SET(interface_units "${interface_units},\n\t\"${unit}\"")
    ENDFOREACH()    

    FOREACH(unit ${adb_units})
      SET(source_files "${source_files},\n\t\"${unit}\"")
    ENDFOREACH()    


    if (WIN32)
      set(ADA_OUTPUT "${dots_generated_SOURCE_DIR}/ada/${CC_prefix}/lib/libdots_generated_${CC_prefix}_ada.dll")
    elseif (UNIX)
      set(ADA_OUTPUT "${dots_generated_SOURCE_DIR}/ada/${CC_prefix}/lib/libdots_generated_${CC_prefix}_ada.so")
    endif(WIN32)

    #check dependecies between .gpr files
    set(additional_dependecies "")
    set(additional_final_dependecies "")
    if(DEFINED DOTS_NS_${prefix})
      SET(DOTS_TMP_NS DOTS_NS_${prefix})
      FOREACH(DOTS_DEPEND ${${DOTS_TMP_NS}})
        string_to_ada_style(${DOTS_DEPEND} DOTS_ADA_DEPEND)
        set(additional_dependecies "${additional_dependecies}with \"dots_generated_${DOTS_ADA_DEPEND}_ada_library.gpr\";\n")
        set(additional_final_dependecies "${additional_final_dependecies}with \"dots_generated_${DOTS_ADA_DEPEND}_ada.gpr\";\n")
      ENDFOREACH()
    endif()
    
    
    CONFIGURE_FILE (${dots_generated_SOURCE_DIR}/dots_generated_ada_library.gpr_template ${dots_generated_SOURCE_DIR}/ada/dots_generated_${CC_prefix}_ada_library.gpr)

    SET(gnatmake_arguments -XBUILD=${CUSTOM_BUILD_TYPE})
    ADD_CUSTOM_COMMAND(OUTPUT ${ADA_OUTPUT}
      COMMAND gnatmake ${gnatmake_arguments} -P ${dots_generated_SOURCE_DIR}/ada/dots_generated_${CC_prefix}_ada_library.gpr
      WORKING_DIRECTORY ${dots_generated_SOURCE_DIR}
      COMMENT "Building dots_generated_${CC_prefix}_ada using gnatmake" 
      VERBATIM
      )

    ADD_CUSTOM_TARGET(dots_generated_${prefix}_ada ALL
      DEPENDS ${ADA_OUTPUT})
    
    if(DEFINED DOTS_NS_${prefix})
      SET(DOTS_TMP_NS DOTS_NS_${prefix})
      FOREACH(DOTS_DEPEND ${${DOTS_TMP_NS}})
        add_dependencies(dots_generated_${prefix}_ada dots_generated_${DOTS_DEPEND}_ada)
      ENDFOREACH()
    endif()

    CONFIGURE_FILE (${dots_generated_SOURCE_DIR}/dots_generated_ada.gpr_template ${dots_generated_SOURCE_DIR}/ada/dots_generated_${CC_prefix}_ada.gpr)
    
    INSTALL(DIRECTORY ada/${CC_prefix}/interface/
      DESTINATION ${SAFIR_SDK}/ada/dots_generated_${CC_prefix}_ada/interface)
    INSTALL(DIRECTORY ada/${CC_prefix}/ali/
      DESTINATION  ${SAFIR_SDK}/ada/dots_generated_${CC_prefix}_ada/ali)
    INSTALL(FILES ada/dots_generated_${CC_prefix}_ada.gpr
      DESTINATION ${SAFIR_SDK}/ada)
    INSTALL(FILES ada/dots_generated_${CC_prefix}_ada.gpr  # dummy to force uninstall of dots_generated_${CC_prefix}_ada folder.
      DESTINATION ${SAFIR_SDK}/ada/dots_generated_${CC_prefix}_ada)
    if (WIN32)
      INSTALL(FILES ada/${CC_prefix}/lib/libdots_generated_${CC_prefix}_ada.dll
        DESTINATION ${SAFIR_RUNTIME}/bin)
    elseif (UNIX)
      INSTALL(FILES ada/${CC_prefix}/lib/libdots_generated_${CC_prefix}_ada.so
        DESTINATION ${SAFIR_RUNTIME}/lib/)
    endif(WIN32)

  ENDFOREACH()

  ##
  ## Build main dots_generated_ada.gpr
  ##
  set(additional_final_dependecies "")

  FOREACH(prefix ${DOTS_NS})
    string_to_ada_style(${prefix} DOTS_ADA_DEPEND)
    set(additional_final_dependecies "${additional_final_dependecies}with \"dots_generated_${DOTS_ADA_DEPEND}_ada.gpr\";\n")
  ENDFOREACH()
  
  CONFIGURE_FILE (${dots_generated_SOURCE_DIR}/dots_generated_main_ada.gpr_template ${dots_generated_SOURCE_DIR}/ada/dots_generated_ada.gpr)


  INSTALL(FILES ada/dots_generated_ada.gpr
    DESTINATION ${SAFIR_SDK}/ada)
  
endif()

