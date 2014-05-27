function(ADD_CSHARP_ASSEMBLY TARGET_NAME)
    cmake_parse_arguments(_cs "LIBRARY;EXE;WINEXE" "SIGN" "SOURCES;REFERENCES" ${ARGN})
    
    if (NOT _cs_LIBRARY AND NOT _cs_EXE AND NOT _cs_WINEXE)
      message(FATAL_ERROR "ADD_CSHARP_ASSEMBLY: TARGET_KIND not specified!")
    endif()

    if (NOT "${_cs_UNPARSED_ARGUMENTS}" STREQUAL "")
      message(FATAL_ERROR "Unknown argument to SAFIR_INSTALL '${_cs_UNPARSED_ARGUMENTS}'")
    endif()

    #we always generated debug info and enable optimizations, regardless of build type
    SET(_cs_flags "-debug -optimize")

    if (_cs_SIGN)
      set(_cs_flags "${_cs_flags} -keyfile:\"${_cs_SIGN}\"")
    endif()

    set (_cs_doc_file "${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.xml")

    if (_cs_LIBRARY)
      set (_cs_target "${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.dll")
      set (_cs_target_kind library)
    elseif (_cs_EXE)
      set (_cs_target "${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.exe")
      set (_cs_target_kind exe)
    elseif (_cs_WINEXE)
      set (_cs_target "${CMAKE_CURRENT_CURRENT_BINARY_DIR}/${TARGET_NAME}.exe")
      set (_cs_target_kind winexe)
    endif()

    set (_cs_debug_file "${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}${CSHARP_DEBUG_INFO_FILE_SUFFIX}")
    foreach(_cs_ref ${_cs_REFERENCES})
      get_target_property(_cs_ref_file ${_cs_ref} ASSEMBLY_FILE)
      set(references "-reference:\"${_cs_ref_file}\"")
    endforeach()

    SET (response_file ${CMAKE_CURRENT_BINARY_DIR}/command_line_${TARGET_NAME}.rsp)
    string(REPLACE ";" "\"\n\"" _cs_sources_spaced "\"${_cs_SOURCES}\"")
    file (WRITE ${response_file} "${_cs_flags}
                                  -nologo
                                  -out:\"${_cs_target}\"
                                  -target:${_cs_target_kind}
                                  ${references}
                                  -doc:\"${_cs_doc_file}\"
                                  ${_cs_sources_spaced}")
    
    #Log contents if needed
    #if (NOT $ENV{VERBOSE} STREQUAL "")
      FILE(READ ${response_file} response_file_contents)
      MESSAGE("Contents of ${response_file} is ${response_file_contents}")
    #endif()   

    add_custom_command (
      OUTPUT ${_cs_target} ${_cs_doc_file} ${_cs_debug_file} ${response_file}
      COMMAND ${CSHARP_COMPILER} @${response_file}
      DEPENDS ${source}
      COMMENT "Building assembly ${TARGET_NAME}"
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

    ADD_CUSTOM_TARGET (${TARGET_NAME} ALL DEPENDS ${_cs_target})

    #Set some properties on the target
    set_property(TARGET ${TARGET_NAME}
      PROPERTY ASSEMBLY_FILE ${_cs_target})

    set_property(TARGET ${TARGET_NAME}
      PROPERTY DOC_FILE ${_cs_doc_file})
    
    set_property(TARGET ${TARGET_NAME}
      PROPERTY DEBUG_INFO_FILE ${_cs_debug_file})
endfunction()

#TODO: install fcn
