cmake_minimum_required(VERSION 3.11)

if (SAFIR_CSHARP_IS_CMAKE_NATIVE)
  include(CSharpUtilities)
endif()

function(ADD_CSHARP_ASSEMBLY TARGET_NAME)
  cmake_parse_arguments(_cs
    "LIBRARY;EXE;WINEXE;NOVERSION" "SIGN;ICON" "SOURCES;RESOURCES;REFERENCES;LIBRARY_PATHS;DLL_IMPORTS" ${ARGN})

  if (NOT _cs_LIBRARY AND NOT _cs_EXE AND NOT _cs_WINEXE)
    message(FATAL_ERROR "ADD_CSHARP_ASSEMBLY: TARGET_KIND not specified!")
  endif()

  if (NOT "${_cs_UNPARSED_ARGUMENTS}" STREQUAL "")
    message(FATAL_ERROR "Unknown argument to ADD_CSHARP_ASSEMBLY '${_cs_UNPARSED_ARGUMENTS}'")
  endif()

  if (CMAKE_CSHARP_OUTPUT_DIRECTORY)
    set(_cs_output_dir ${CMAKE_CSHARP_OUTPUT_DIRECTORY})
  else()
    set(_cs_output_dir ${CMAKE_CURRENT_BINARY_DIR})
  endif()

  if (_cs_LIBRARY)
    set (_cs_target "${_cs_output_dir}/${TARGET_NAME}.dll")
    set (_cs_target_kind library)
    set (_cs_policy_filename ${TARGET_NAME}.dll.policy)
    set (_cs_policy_file ${_cs_output_dir}/${_cs_policy_filename})
    set (_cs_policy_assembly_name "Policy.${SAFIR_VERSION_MAJOR}.${SAFIR_VERSION_MINOR}.${TARGET_NAME}.dll")
    set (_cs_policy_assembly_file "${_cs_output_dir}/${_cs_policy_assembly_name}")
    set (_cs_doc_file "${_cs_output_dir}/${TARGET_NAME}.xml")
  elseif (_cs_EXE)
    set (_cs_target "${_cs_output_dir}/${TARGET_NAME}.exe")
    set (_cs_target_kind exe)
  elseif (_cs_WINEXE)
    set (_cs_target "${_cs_output_dir}/${TARGET_NAME}.exe")
    set (_cs_target_kind winexe)
  endif()

  if (NOT _cs_NOVERSION)
    add_custom_command(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/version.cs
      COMMAND ${CMAKE_COMMAND} -E echo "//This is an automatically generated file." > ${CMAKE_CURRENT_BINARY_DIR}/version.cs
      COMMAND ${CMAKE_COMMAND} -E echo "[assembly: System.Reflection.AssemblyVersion\(\"${SAFIR_VERSION_MAJOR}.${SAFIR_VERSION_MINOR}.${SAFIR_VERSION_PATCH}.0\"\)]" >> ${CMAKE_CURRENT_BINARY_DIR}/version.cs
      COMMAND ${CMAKE_COMMAND} -E echo "internal class BuildInfo {public const string Version = \"${SAFIR_VERSION_MAJOR}.${SAFIR_VERSION_MINOR}.${SAFIR_VERSION_PATCH}.0\";}" >> ${CMAKE_CURRENT_BINARY_DIR}/version.cs
      COMMENT "Creating version.cs for ${TARGET_NAME}"
      VERBATIM)

    #add version.cs to the list of sources, but remove it again if it was already there...
    LIST(APPEND _cs_SOURCES ${CMAKE_CURRENT_BINARY_DIR}/version.cs)
    LIST(REMOVE_DUPLICATES _cs_SOURCES)

    if (_cs_LIBRARY AND _cs_SIGN)
      execute_process(COMMAND ${SN_EXECUTABLE} -q -p ${_cs_SIGN} ${CMAKE_CURRENT_BINARY_DIR}/pubkey.tmp OUTPUT_VARIABLE _cs_sn_output RESULT_VARIABLE _cs_sn_first_result)
      execute_process(COMMAND ${SN_EXECUTABLE} -q -t ${CMAKE_CURRENT_BINARY_DIR}/pubkey.tmp OUTPUT_VARIABLE _cs_sn_output RESULT_VARIABLE _cs_sn_second_result)
      string(REGEX MATCH "Public (K|k)ey (T|t)oken(:| is) ([0-9a-fA-F]*)" _cs_sn_token ${_cs_sn_output})
      set(_cs_sn_token ${CMAKE_MATCH_4})
      if (_cs_sn_first_result OR _cs_sn_second_result OR NOT _cs_sn_token)
        message(FATAL_ERROR "Could not extract public key token for target ${TARGET_NAME}")
      endif()

      string(CONCAT _cs_policy_content
        "<?xml version=\"1.0\" encoding=\"utf-8\" ?> "
        "<configuration> "
        "  <runtime> "
        "    <assemblyBinding xmlns=\"urn:schemas-microsoft-com:asm.v1\"> "
        "      <dependentAssembly> "
        "        <assemblyIdentity name=\"${TARGET_NAME}\" "
        "                          publicKeyToken=\"${_cs_sn_token}\" "
        "                          culture=\"neutral\" /> "
        "        <bindingRedirect oldVersion=\"${SAFIR_VERSION_MAJOR}.${SAFIR_VERSION_MINOR}.0.0-${SAFIR_VERSION_MAJOR}.${SAFIR_VERSION_MINOR}.${SAFIR_VERSION_PATCH}.0\" "
        "                         newVersion=\"${SAFIR_VERSION_MAJOR}.${SAFIR_VERSION_MINOR}.${SAFIR_VERSION_PATCH}.0\"/> "
        "      </dependentAssembly> "
        "    </assemblyBinding> "
        "  </runtime> "
        "</configuration> ")

      add_custom_command(
        OUTPUT ${_cs_policy_file}
        COMMAND ${CMAKE_COMMAND} -E echo "${_cs_policy_content}" > ${_cs_policy_file}
        COMMENT "Creating publisher policy for ${TARGET_NAME}"
        VERBATIM)

      add_custom_command (
        OUTPUT ${_cs_policy_assembly_file}
        COMMAND ${CSHARP_LINKER}
                   -nologo
                   -link:${_cs_policy_filename}
                   -out:${_cs_policy_assembly_name}
                   -keyfile:${_cs_SIGN}
                   ${CSHARP_PLATFORM_FLAG}
                   -v:${SAFIR_VERSION_MAJOR}.${SAFIR_VERSION_MINOR}.${SAFIR_VERSION_PATCH}.0
        DEPENDS ${_cs_policy_file}
        COMMENT "Building Publisher Policy for ${TARGET_NAME}"
        WORKING_DIRECTORY ${_cs_output_dir})

      add_custom_target (policy.${TARGET_NAME} ALL DEPENDS ${_cs_policy_assembly_file} SOURCES ${_cs_policy_file})

    endif()

  endif()

  if (SAFIR_CSHARP_IS_CMAKE_NATIVE)
    SET(CMAKE_DEBUG_POSTFIX "")

    foreach(resx_file ${_cs_RESOURCES})
      if (resx_file STREQUAL "PREFIX")
        set(get_resource_prefix True)
      elseif(get_resource_prefix)
        unset(get_resource_prefix)
        set(resource_prefix ${resx_file})
      elseif(resx_file STREQUAL "WORKING_DIRECTORY")
        set(get_working_directory True)
      elseif (get_working_directory)
        unset(get_working_directory)
        set(working_directory ${resx_file})
      else()
        list(APPEND resx_files ${resx_file})
      endif()
    endforeach()

    if (_cs_LIBRARY)
      add_library(${TARGET_NAME} SHARED ${_cs_SOURCES} ${resx_files})

      set_target_properties(${TARGET_NAME} PROPERTIES
        VS_DOTNET_DOCUMENTATION_FILE "${_cs_doc_file}")
    elseif (_cs_EXE)
      add_executable(${TARGET_NAME} ${_cs_SOURCES} ${resx_files})
    elseif (_cs_WINEXE)
      add_executable(${TARGET_NAME} WIN32 ${_cs_SOURCES} ${resx_files})
    endif()

    set_target_properties(${TARGET_NAME} PROPERTIES
      RUNTIME_OUTPUT_DIRECTORY "${_cs_output_dir}")

    if (_cs_SIGN)
      set_target_properties(${TARGET_NAME} PROPERTIES
        VS_GLOBAL_SignAssembly "true"
        VS_GLOBAL_AssemblyOriginatorKeyFile "${_cs_SIGN}")
    endif()

    if (_cs_ICON)
      set_target_properties(${TARGET_NAME} PROPERTIES
        VS_GLOBAL_ApplicationIcon "${_cs_ICON}")
    endif()

    #add assembly references
    foreach(_cs_ref ${_cs_REFERENCES})
      if (TARGET ${_cs_ref})
        add_dependencies(${TARGET_NAME} ${_cs_ref})
      else()
        list(APPEND _cs_dotnet_references ${_cs_ref})
      endif()
    endforeach()

    set_property(TARGET ${TARGET_NAME}
      PROPERTY VS_DOTNET_REFERENCES System Microsoft.CSharp System.EnterpriseServices ${_cs_dotnet_references})

    #add dependencies to dll-imported stuff, but with a dummy target in between, to
    #avoid the dll becoming a reference in the C# project
    add_custom_target(${TARGET_NAME}_native_deps DEPENDS ${_cs_DLL_IMPORTS})
    add_dependencies(${TARGET_NAME} ${TARGET_NAME}_native_deps)


    set_property(TARGET ${TARGET_NAME}
      PROPERTY ASSEMBLY_FILE $<TARGET_FILE:${TARGET_NAME}>)

    if (_cs_RESOURCES)
      csharp_set_windows_forms_properties(${_cs_SOURCES} ${resx_files})
      set_source_files_properties(${resx_files} PROPERTIES VS_RESOURCE_GENERATOR "")
    endif()

    #if (resource_prefix)
    #    set_property(TARGET ${TARGET_NAME} PROPERTY VS_GLOBAL_RootNamespace ${resource_prefix})
    #endif()

    set (_cs_debug_file "$<TARGET_PDB_FILE:${TARGET_NAME}>")

  else (SAFIR_CSHARP_IS_CMAKE_NATIVE)
    #we always generated debug info and enable optimizations, regardless of build type
    SET(_cs_flags "${CSHARP_COMPILER_FLAGS} ${CSHARP_PLATFORM_FLAG} -debug -optimize -fullpaths")

    #add docs for libraries
    if (_cs_LIBRARY)
      set (_cs_flags "${_cs_flags} -doc:\"${_cs_doc_file}\"")
    endif()

    #On Windows we want to ensure that we target a specific version of the .NET framework, so we
    #point it out specifically. On Linux it doesnt matter so much, since we target whatever
    #is in the distro repos.
    #This protects us from Windows Update sabotaging our build machines by installing
    #a new .Net Framework version overnight...
    if (WIN32 AND NOT SAFIR_EXTERNAL_BUILD)
      SET(_cs_flags "${_cs_flags} ${CSHARP_COMPILER_FRAMEWORK_ARGUMENTS}")

      #This has to appear on the command line, not in the response file, hence we keep
      #it separate
      SET(_cs_noconfig -noconfig)
    endif()


    if (_cs_SIGN)
      set(_cs_flags "${_cs_flags} -keyfile:\"${_cs_SIGN}\"")
    endif()

    if (_cs_ICON)
      get_filename_component(_cs_ICON ${_cs_ICON} ABSOLUTE)
      set (_cs_flags "${_cs_flags} -win32icon:\"${_cs_ICON}\"")
    endif()

    #on msvc the debug files will be named MyAssembly.pdb, but with mono they will be
    #named MyAssembly.dll.mdb
    if (CSHARP_IS_MONO)
      set (_cs_debug_file "${_cs_target}.mdb")
    else()
      set (_cs_debug_file "${_cs_output_dir}/${TARGET_NAME}.pdb")
    endif()

    foreach(resx_file ${_cs_RESOURCES})
      if (resx_file STREQUAL "PREFIX")
        set(get_resource_prefix True)
        unset(working_directory)
      elseif(get_resource_prefix)
        unset(get_resource_prefix)
        set (resource_prefix ${resx_file})
      elseif(resx_file STREQUAL "WORKING_DIRECTORY")
        set(get_working_directory True)
      elseif (get_working_directory)
        unset(get_working_directory)
        set(working_directory ${resx_file})
      else()
        get_filename_component(base_name ${resx_file} NAME_WE)
        set (resources_file "${CMAKE_CURRENT_BINARY_DIR}/${resource_prefix}${base_name}.resources")
        set (_cs_resources_files ${_cs_resources_files} ${resources_file})
        if (WIN32)
          file (TO_NATIVE_PATH ${resources_file} resources_file_native)
        else()
          set (resources_file_native ${resources_file})
        endif()


        set (_cs_resources_cmd "${_cs_resources_cmd} -res:\"${resources_file_native}\"")

        #if we're not verbose we redirect output to null.
        if ("$ENV{VERBOSE}" STREQUAL "")
          if (WIN32)
            set(redirect_output > /nul)
          else()
            set(redirect_output > /dev/null)
          endif()
        endif()


        ADD_CUSTOM_COMMAND(OUTPUT ${resources_file}
          COMMAND ${RESGEN_EXECUTABLE} ARGS ${resx_file} ${resources_file_native} ${redirect_output}
          DEPENDS ${resx_file}
          WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${working_directory}
        )
      endif()
    endforeach()

    foreach(_cs_ref ${_cs_REFERENCES})

      if (TARGET ${_cs_ref})
        get_target_property(_cs_ref_file ${_cs_ref} ASSEMBLY_FILE)
        set(references "${references} -reference:\"${_cs_ref_file}\"")
        set(_cs_target_dependencies ${_cs_target_dependencies} ${_cs_ref})
      else()
        set(references "${references} -reference:\"${_cs_ref}.dll\"")
      endif()
    endforeach()

    if (_cs_LIBRARY_PATHS OR CSHARP_LIBRARY_PATHS)
      set(_cs_lib_arg "-lib:")
    endif()

    foreach(path IN LISTS _cs_LIBRARY_PATHS CSHARP_LIBRARY_PATHS)
      set(_cs_lib_arg "${_cs_lib_arg}\"${path}\",")
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
                                      ${_cs_lib_arg}
                                      ${_cs_resources_cmd}
                                      ${_cs_sources_spaced}")

    #Log contents if needed
    if (NOT "$ENV{VERBOSE}" STREQUAL "")
      FILE(READ "${response_file}" response_file_contents)
      MESSAGE("Contents of ${response_file} is ${response_file_contents}")
    endif()


    add_custom_command (
      OUTPUT ${_cs_target} ${_cs_doc_file} ${_cs_debug_file}
      COMMAND ${CSHARP_COMPILER} ${_cs_noconfig} @${response_file}
      DEPENDS ${_cs_SOURCES} ${_cs_target_dependencies} ${_cs_resources_files}
      COMMENT "Building ${_cs_target_kind} assembly ${TARGET_NAME}"
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

    ADD_CUSTOM_TARGET (${TARGET_NAME} ALL DEPENDS ${_cs_target} SOURCES ${_cs_SOURCES})

    #we need to write a dll map file for linux distros that split stuff into a -dev package
    if (UNIX AND _cs_DLL_IMPORTS)
      FILE(WRITE ${_cs_target}.config
        "<configuration>\n")
      foreach(dll IN LISTS _cs_DLL_IMPORTS)
        FILE(APPEND ${_cs_target}.config
          "  <dllmap dll=\"${dll}\" target=\"lib${dll}.so.${SAFIR_VERSION_MAJOR}\"/>\n")
      endforeach()
      FILE(APPEND ${_cs_target}.config
        "</configuration>")

      set_property(TARGET ${TARGET_NAME}
        PROPERTY CONFIG_FILE ${_cs_target}.config)
    endif()

    #add dependencies to dll-imported stuff
    foreach(dll IN LISTS _cs_DLL_IMPORTS)
      add_dependencies(${TARGET_NAME} ${dll})
    endforeach()

    set_property(TARGET ${TARGET_NAME}
      PROPERTY ASSEMBLY_FILE ${_cs_target})
  endif(SAFIR_CSHARP_IS_CMAKE_NATIVE)

  #Set some properties on the target

  if (_cs_LIBRARY)
    set_property(TARGET ${TARGET_NAME}
      PROPERTY DOC_FILE ${_cs_doc_file})
  endif()

  if (_cs_LIBRARY AND _cs_SIGN AND NOT _cs_NOVERSION)
    set_property(TARGET ${TARGET_NAME}
      PROPERTY PUBLISHER_POLICY_FILE ${_cs_policy_assembly_file} ${_cs_policy_file})
  endif()

  set_property(TARGET ${TARGET_NAME}
    PROPERTY TARGET_KIND ${_cs_target_kind})

  set_property(TARGET ${TARGET_NAME}
    PROPERTY DEBUG_INFO_FILE ${_cs_debug_file})

endfunction()


function(INSTALL_CSHARP_ASSEMBLY)
  cmake_parse_arguments(_cs "" "TARGET;DESTINATION;COMPONENT" "" ${ARGN})

  if (NOT "${_cs_UNPARSED_ARGUMENTS}" STREQUAL "")
    message(FATAL_ERROR "Unknown argument to INSTALL_CSHARP_ASSEMBLY '${_cs_UNPARSED_ARGUMENTS}'")
  endif()

  #check component and use Runtime if it was not specified explicitly
  if (NOT _cs_COMPONENT)
    set(_cs_COMPONENT Runtime)
  endif()
  set (_components Runtime Tools Development TestSuite)
  list(FIND _components "${_cs_COMPONENT}" _valid_component)
  if(_valid_component EQUAL -1)
    message(FATAL_ERROR "Invalid COMPONENT '${_cs_COMPONENT}'")
  endif()

  if (NOT TARGET ${_cs_TARGET})
    message(FATAL_ERROR "The target ${_TARGET_NAME} is not known in this scope.")
    return()
  endif()

  get_property(_cs_ASSEMBLY_FILE TARGET ${_cs_TARGET} PROPERTY ASSEMBLY_FILE)
  get_property(_cs_TARGET_KIND TARGET ${_cs_TARGET} PROPERTY TARGET_KIND)
  get_property(_cs_DOC_FILE TARGET ${_cs_TARGET} PROPERTY DOC_FILE)
  get_property(_cs_PUBLISHER_POLICY_FILE TARGET ${_cs_TARGET} PROPERTY PUBLISHER_POLICY_FILE)
  get_property(_cs_DEBUG_INFO_FILE TARGET ${_cs_TARGET} PROPERTY DEBUG_INFO_FILE)
  get_property(_cs_CONFIG_FILE TARGET ${_cs_TARGET} PROPERTY CONFIG_FILE)

  set(_cs_COMPONENT_RUNTIME ${_cs_COMPONENT})
  set(_cs_COMPONENT_DEVELOPMENT ${_cs_COMPONENT})

  #runtime and tools targets still want some stuff put in the development component.
  if (_cs_COMPONENT STREQUAL "Runtime" OR _cs_COMPONENT STREQUAL "Tools")
    set(_cs_COMPONENT_DEVELOPMENT "Development")
  endif()

  if (_cs_TARGET_KIND STREQUAL "library")
    install(FILES ${_cs_ASSEMBLY_FILE} ${_cs_PUBLISHER_POLICY_FILE}
      DESTINATION ${_cs_DESTINATION}
      COMPONENT ${_cs_COMPONENT_RUNTIME})

    install(FILES ${_cs_DOC_FILE}
      DESTINATION ${_cs_DESTINATION}
      COMPONENT ${_cs_COMPONENT_DEVELOPMENT})

    if (_cs_COMPONENT STREQUAL "Runtime" OR _cs_COMPONENT STREQUAL "Development" OR _cs_COMPONENT STREQUAL "")
        install(FILES ${_cs_ASSEMBLY_FILE} ${_cs_PUBLISHER_POLICY_FILE} ${_cs_DOC_FILE}
          DESTINATION lib/netstandard2.0/
          COMPONENT NuGet EXCLUDE_FROM_ALL)
    endif()
  else()
    install(PROGRAMS ${_cs_ASSEMBLY_FILE}
      DESTINATION ${_cs_DESTINATION}
      COMPONENT ${_cs_COMPONENT_RUNTIME})
  endif()

  install(FILES ${_cs_DEBUG_INFO_FILE}
    DESTINATION ${_cs_DESTINATION}
    COMPONENT ${_cs_COMPONENT_DEVELOPMENT})

  if (_cs_CONFIG_FILE)
    install(FILES ${_cs_CONFIG_FILE}
      DESTINATION ${destination}
      COMPONENT ${_cs_COMPONENT_RUNTIME})
  endif()
endfunction()
