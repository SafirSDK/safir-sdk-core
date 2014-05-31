function(ADD_CSHARP_ASSEMBLY TARGET_NAME)
    cmake_parse_arguments(_cs "LIBRARY;EXE;WINEXE" "SIGN;RESOURCE_PREFIX" "SOURCES;RESOURCES;REFERENCES" ${ARGN})
    
    if (NOT _cs_LIBRARY AND NOT _cs_EXE AND NOT _cs_WINEXE)
      message(FATAL_ERROR "ADD_CSHARP_ASSEMBLY: TARGET_KIND not specified!")
    endif()

    if (NOT "${_cs_UNPARSED_ARGUMENTS}" STREQUAL "")
      message(FATAL_ERROR "Unknown argument to SAFIR_INSTALL '${_cs_UNPARSED_ARGUMENTS}'")
    endif()

    #we always generated debug info and enable optimizations, regardless of build type
    SET(_cs_flags "-debug -optimize -fullpaths -define:FUNC_PTR_WORKAROUND")

    if (_cs_SIGN)
      set(_cs_flags "${_cs_flags} -keyfile:\"${_cs_SIGN}\"")
    endif()

    set (_cs_doc_file "${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.xml")

    if (_cs_LIBRARY)
      set (_cs_target "${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.dll")
      set (_cs_target_kind library)
      set (_cs_flags "${_cs_flags} -doc:\"${_cs_doc_file}\"")
    elseif (_cs_EXE)
      set (_cs_target "${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.exe")
      set (_cs_target_kind exe)
    elseif (_cs_WINEXE)
      set (_cs_target "${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.exe")
      set (_cs_target_kind winexe)
    endif()

    #on msvc the debug files will be named MyAssembly.pdb, but with mono they will be 
    #named MyAssembly.dll.mdb
    if (CSHARP_IS_MONO)
      set (_cs_debug_file "${_cs_target}.mdb")
    else()
      set (_cs_debug_file "${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.pdb")
    endif()

    foreach(resx_file ${_cs_RESOURCES})
      get_filename_component(base_name ${resx_file} NAME_WE)
      set (resources_file "${CMAKE_CURRENT_BINARY_DIR}/${_cs_RESOURCE_PREFIX}${base_name}.resources")
      set (_cs_resources_files ${_cs_resources_files} ${resources_file})
      set (_cs_resources_cmd "${_cs_resources_cmd} -res:${resources_file}")
      
      ADD_CUSTOM_COMMAND(OUTPUT ${resources_file}
        COMMAND ${RESGEN_EXECUTABLE} ARGS ${resx_file} ${resources_file}
        DEPENDS ${resx_file}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        )
    endforeach()

    foreach(_cs_ref ${_cs_REFERENCES})
      #This disables a warning about getting properties for targets that dont exist
      #which is exactly what we do below.
      cmake_policy(SET CMP0045 OLD)

      get_target_property(_cs_ref_file ${_cs_ref} ASSEMBLY_FILE)
      if (_cs_ref_file)
        set(references "${references} -reference:\"${_cs_ref_file}\"")
        set(_cs_target_dependencies ${_cs_target_dependencies} ${_cs_ref})
      else()
        set(references "${references} -reference:\"${_cs_ref}\"")
      endif()
      #set(ref_depends ${ref_depends} ${_cs_ref_file})
    endforeach()

    SET (response_file ${CMAKE_CURRENT_BINARY_DIR}/command_line_${TARGET_NAME}.rsp)
    foreach(src ${_cs_SOURCES})
      if (WIN32)
        file (TO_NATIVE_PATH ${src} _cs_source_native)
        set (_cs_sources_native ${_cs_sources_native} "${_cs_source_native}")
      else()
        set (_cs_sources_native ${_cs_sources_native} "${src}")
      endif()
    endforeach()

    string(REPLACE ";" "\"\n\"" _cs_sources_spaced "\"${_cs_sources_native}\"")
    file (WRITE ${response_file} "${_cs_flags}
                                  -nologo
                                  -out:\"${_cs_target}\"
                                  -target:${_cs_target_kind}
                                  ${references}
                                  ${_cs_resources_cmd}
                                  ${_cs_sources_spaced}")
    
    #Log contents if needed
    if (NOT $ENV{VERBOSE} STREQUAL "")
      FILE(READ ${response_file} response_file_contents)
      MESSAGE("Contents of ${response_file} is ${response_file_contents}")
    endif()   


    add_custom_command (
      OUTPUT ${_cs_target} ${_cs_doc_file} ${_cs_debug_file}
      COMMAND ${CSHARP_COMPILER} @${response_file}
      DEPENDS ${_cs_SOURCES} ${_cs_target_dependencies} ${_cs_resources_files}
      COMMENT "Building ${_cs_target_kind} assembly ${TARGET_NAME}"
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

    ADD_CUSTOM_TARGET (${TARGET_NAME} ALL DEPENDS ${_cs_target})

    #Set some properties on the target
    set_property(TARGET ${TARGET_NAME}
      PROPERTY ASSEMBLY_FILE ${_cs_target})

    set_property(TARGET ${TARGET_NAME}
      PROPERTY DOC_FILE ${_cs_doc_file})

    set_property(TARGET ${TARGET_NAME}
      PROPERTY TARGET_KIND ${_cs_target_kind})

    if (_cs_LIBRARY)
      set_property(TARGET ${TARGET_NAME}
        PROPERTY DEBUG_INFO_FILE ${_cs_debug_file})
    endif()

endfunction()

#TODO: install fcn
