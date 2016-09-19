#******************************************************************************
#
# Copyright Saab AB, 2014-2015 (http://safirsdkcore.com)
#
# Created by: Lars Hagström <lars.hagstrom@consoden.se>
#
#******************************************************************************
#
# This file is part of Safir SDK Core.
#
# Safir SDK Core is free software: you can redistribute it and/or modify
# it under the terms of version 3 of the GNU General Public License as
# published by the Free Software Foundation.
#
# Safir SDK Core is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
#
#******************************************************************************
include(CMakeParseArguments)

# Documentation for this function can be found in SafirSDKCoreConfig.cmake
FUNCTION(ADD_SAFIR_GENERATED_LIBRARY)
  cmake_parse_arguments(_gen "GLOB" "NAME" "DOU_FILES;DOM_FILES;NAMESPACE_MAPPINGS;DEPENDENCIES" ${ARGN})

  if ("${_gen_NAME}" STREQUAL "")
    message(FATAL_ERROR "Invalid NAME passed to ADD_SAFIR_GENERATED_LIBRARY")
  endif()

  if (NOT "${_gen_UNPARSED_ARGUMENTS}" STREQUAL "")
    message(FATAL_ERROR "Unknown argument to ADD_SAFIR_GENERATED_LIBRARY '${_gen_UNPARSED_ARGUMENTS}'")
  endif()

  if (_gen_GLOB AND (NOT "${_gen_DOU_FILES}" STREQUAL "" OR
                     NOT "${_gen_DOM_FILES}" STREQUAL "" OR
                     NOT "${_gen_NAMESPACE_MAPPINGS}" STREQUAL ""))
    message(FATAL_ERROR "ADD_SAFIR_GENERATED_LIBRARY: the GLOB option cannot be used together with the\n"
                        "DOU_FILES, DOM_FILES or NAMESPACE_MAPPINGS options.")
  endif()

  # Work out if we're building the Safir SDK Core source tree or not
  SAFIR_IS_EXTERNAL_BUILD()

  if (SAFIR_EXTERNAL_BUILD)
    #load compiler settings, csharp and java!
    include(${SAFIR_SDK_CORE_CMAKE_DIR}/SafirCompilerSettings.cmake)
    include(${SAFIR_SDK_CORE_CMAKE_DIR}/PrecompiledHeader.cmake)
    include(${SAFIR_SDK_CORE_DOTNET_SETTINGS})
    include(${SAFIR_SDK_CORE_JAVA_SETTINGS})
    #We need boost headers.
    set(Boost_FIND_QUIETLY True)
    find_package(Boost COMPONENTS system thread REQUIRED)
    include_directories(${Boost_INCLUDE_DIRS})
    link_directories(${Boost_LIBRARY_DIRS})

    #use dynamic linking with boost
    ADD_DEFINITIONS(-DBOOST_ALL_DYN_LINK)

    #Debug dlls on windows need d suffix
    if (MSVC)
      SET(CMAKE_DEBUG_POSTFIX "d")
    endif()
  else()
    include(PrecompiledHeader)
  endif()

  #
  # Dependency resolution
  #
  # Create custom targets dummy that we just use to have somewhere to put the dependencies.
  # Then, when we need to resolve a targets dependencies, we recursively look at the dummy
  # targets to find all of the dependencies of our dependencies.
  add_custom_target(${_gen_NAME}-dou)
  set_target_properties(${_gen_NAME}-dou PROPERTIES
    DOU_DEPENDENCIES "${_gen_DEPENDENCIES}"
    DOU_DIR ${CMAKE_CURRENT_SOURCE_DIR})

  #recursively get all dependendencies
  function(GET_DEPS)
    cmake_parse_arguments(DEP "" "" "DEPENDENCIES" ${ARGN})

    FOREACH (DEP ${DEP_DEPENDENCIES})
      if (NOT "${DEP}" STREQUAL "")
        if (TARGET ${DEP}-dou)
          get_target_property(deps ${DEP}-dou DOU_DEPENDENCIES)
          if (NOT "${deps}" STREQUAL "deps-NOTFOUND")
            GET_DEPS(DEPENDENCIES ${deps})
          endif()
        else()
          if (NOT SAFIR_EXTERNAL_BUILD)
            message(FATAL_ERROR "External dependencies in Core build tree is not allowed!")
          endif()
          execute_process(
            COMMAND safir_show_config --module-dependencies ${DEP}
            RESULT_VARIABLE ext_dep_res
            OUTPUT_VARIABLE ext_dep
            ERROR_VARIABLE ext_dep)
          if (ext_dep_res EQUAL 0)
            string(STRIP ${ext_dep} ext_dep)
            string(REPLACE " " ";" ext_dep "${ext_dep}")
            GET_DEPS(DEPENDENCIES ${ext_dep})
          else()
            message(FATAL_ERROR "Failed to resolve external dependency ${DEP} in typesystem.ini. "
              "Error(${ext_dep_res}):\n${ext_dep}")
          endif()
        endif()
        list (APPEND GET_DEPS_RESULT ${DEP})
      endif()
    ENDFOREACH()
    if(GET_DEPS_RESULT)
      LIST(REMOVE_DUPLICATES GET_DEPS_RESULT)
    endif()
    set (GET_DEPS_RESULT ${GET_DEPS_RESULT} PARENT_SCOPE)
  endfunction()

  GET_DEPS(DEPENDENCIES ${_gen_DEPENDENCIES})
  set(ALL_DEPENDENCIES ${GET_DEPS_RESULT})
  ################

  if (_gen_GLOB)
    #
    # Set up variables containing all dou files and all expected source code files
    #
    FILE(GLOB_RECURSE _gen_DOU_FILES *.dou)
    FILE(GLOB_RECURSE _gen_DOM_FILES *.dom)
    FILE(GLOB_RECURSE _gen_NAMESPACE_MAPPINGS *.namespace.txt)
  endif()

  if ("${_gen_DOU_FILES}" STREQUAL "")
    message(FATAL_ERROR "No dou files found")
  endif()

  #put the files in a target property so the INSTALL_SAFIR_GENERATED_LIBRARY
  #function can know what files it needs to install
  set_property(TARGET ${_gen_NAME}-dou PROPERTY
    SOURCE_FILES ${_gen_DOU_FILES} ${_gen_DOM_FILES} ${_gen_NAMESPACE_MAPPINGS})

  #set up java namespace prefixing rules
  foreach (file ${_gen_NAMESPACE_MAPPINGS})
    get_filename_component(filename ${file} NAME)
    string (REGEX REPLACE "([a-zA-Z\\.0-9]*)-java\\.namespace\\.txt" "\\1" namespace ${filename})

    file(STRINGS ${file} prefix REGEX "^[a-zA-Z0-9\\.]+$") #read the line we want from the file
    set (java_namespace_keys ${java_namespace_keys} ${namespace})
    set (java_namespace_${namespace}_replacement ${prefix}.${namespace})
  endforeach()

  if(java_namespace_keys)
    #get them in reverse order so that 'a.b' will be replaced before 'a'
    list(SORT java_namespace_keys)
    list(REVERSE java_namespace_keys)
  endif()

  #loop over all dou files
  foreach (dou ${_gen_DOU_FILES})
    get_filename_component(douname ${dou} NAME)
    string (REPLACE ".dou" "" base_name ${douname})
    set (cpp_files ${cpp_files} gen/cpp/${base_name}.cpp)
    set (dotnet_files ${dotnet_files} "${CMAKE_CURRENT_BINARY_DIR}/gen/dotnet/${base_name}.cs")

    string (REGEX REPLACE "^([a-zA-Z\\.0-9]*)\\.[a-zA-Z0-9_]+$" "\\1" namespace ${base_name})
    string (REGEX REPLACE "^[a-zA-Z\\.0-9]*\\.([a-zA-Z0-9_]+)$" "\\1" java_base_name ${base_name})

    #perform prefix insertion
    foreach(key ${java_namespace_keys})
      string (REGEX REPLACE "^${key}" "${java_namespace_${key}_replacement}" namespace ${namespace})
    endforeach()


    string (TOLOWER ${namespace} java_namespace)
    string (REPLACE "." "/" java_path ${java_namespace})
    set (java_files ${java_files}
      "${CMAKE_CURRENT_BINARY_DIR}/gen/java/src/${java_path}/${java_base_name}.java")
  endforeach()

  ##############


  #
  # Generate code
  #
  # We set up some variables to point to dots_v and dod-files and directories,
  # and then we define the custom command that generates the code
  #
  foreach(DEP ${ALL_DEPENDENCIES})
    if (TARGET ${DEP}-dou)
      get_target_property(sd ${DEP}-dou DOU_DIR)
      list(APPEND DOTS_V_DEPS ${DEP}=${sd})
    else()
      list(APPEND DOTS_V_DEPS ${DEP})
    endif()
  endforeach()

  FIND_PACKAGE(PythonInterp)

  if (SAFIR_EXTERNAL_BUILD)
    #on linux we install the dots_v script without the extension, so we have to mess about a bit here
    if (MSVC)
      set(dots_v_path "${SAFIR_SDK_CORE_EXECUTABLES_DIR}/dots_v.py")
    else()
      set(dots_v_path "${SAFIR_SDK_CORE_EXECUTABLES_DIR}/dots_v")
    endif()
    set(dod_directory "${SAFIR_SDK_CORE_GENERATION_DIR}/dod/")
  else()
    set(dots_v_path "${safir-sdk-core_SOURCE_DIR}/src/dots/dots_v.ss/dots_v.py")
    set(dod_directory "${safir-sdk-core_SOURCE_DIR}/src/dots/dots_v.ss/data/")
  endif()

  FILE(GLOB dod_files ${dod_directory}*.dod)
  SET(dots_v_command ${PYTHON_EXECUTABLE} ${dots_v_path})
  SET(dots_v_arguments
    --dod-files ${dod_files}
    --dou-files ${_gen_DOU_FILES}
    --namespace-mappings ${_gen_NAMESPACE_MAPPINGS}
    --dependencies ${DOTS_V_DEPS}
    --library-name ${_gen_NAME}
    --output-path=${CMAKE_CURRENT_BINARY_DIR}/gen)

  list(LENGTH _gen_DOU_FILES _gen_DOU_FILES_length)
  if (_gen_DOU_FILES_length GREATER 100)
    list(APPEND dots_v_arguments --multiprocess)
  endif()

  #write the command to a response file, since there may be maaaannnyyy dou
  #files and windows has problems with long command lines
  SET (response_file "${CMAKE_CURRENT_BINARY_DIR}/command_line_dots_v_${_gen_NAME}.rsp")
  file(REMOVE ${response_file})
  foreach(arg IN LISTS dots_v_arguments)
    file(APPEND ${response_file} "${arg}\n")
  endforeach()

  ADD_CUSTOM_COMMAND(
    OUTPUT ${cpp_files} ${java_files} ${dotnet_files}

    COMMAND ${dots_v_command} "@${response_file}"
    DEPENDS ${dod_files} ${_gen_DOU_FILES} ${_gen_NAMESPACE_MAPPINGS}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Generating code for safir_generated-${_gen_NAME}")

  #make clean target remove the gen directory
  SET_DIRECTORY_PROPERTIES(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES gen)

  #We need a custom target that the library (java,cpp,dotnet) targets can depend on, since
  #having them all just depend on the output files will wreak havoc with cmake in parallel builds.
  #See http://public.kitware.com/Bug/view.php?id=12311
  add_custom_target(safir_generated-${_gen_NAME}-code ALL DEPENDS ${cpp_files} ${java_files} ${dotnet_files}
                    SOURCES ${_gen_DOU_FILES} ${_gen_DOM_FILES} ${_gen_NAMESPACE_MAPPINGS})
  #############

  #
  # Build CPP
  #

  #start by setting up some precompiled header stuff
  if (SAFIR_EXTERNAL_BUILD)
    set(precompiled_header_path ${SAFIR_SDK_CORE_GENERATION_DIR}/cpp/)
  else()
    set(precompiled_header_path ${safir-sdk-core_SOURCE_DIR}/src/dots/dots_v.ss/data/)
  endif()

  #MSVC needs an extra source file, so we generate one
  if (MSVC)
    add_custom_command(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/precompiled_header_for_cpp.cpp
      COMMAND ${CMAKE_COMMAND} -E echo "//This is an automatically generated file." > ${CMAKE_CURRENT_BINARY_DIR}/precompiled_header_for_cpp.cpp
      COMMAND ${CMAKE_COMMAND} -E echo "#include <precompiled_header_for_cpp.h>" >> ${CMAKE_CURRENT_BINARY_DIR}/precompiled_header_for_cpp.cpp
      COMMENT "Creating precompiled_header_for_cpp.cpp for ${_gen_NAME}"
      VERBATIM)
    list (APPEND cpp_files ${CMAKE_CURRENT_BINARY_DIR}/precompiled_header_for_cpp.cpp)
  endif()

  #add safir link dirs.
  if (SAFIR_EXTERNAL_BUILD)
    link_directories(${SAFIR_SDK_CORE_LIBRARIES_DIR})
  endif()

  ADD_LIBRARY(safir_generated-${_gen_NAME}-cpp SHARED ${cpp_files})

  target_include_directories(safir_generated-${_gen_NAME}-cpp BEFORE
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/gen/cpp/include>)

  #add safir include dirs.
  if (SAFIR_EXTERNAL_BUILD)
    target_include_directories(safir_generated-${_gen_NAME}-cpp
      PRIVATE
      ${SAFIR_SDK_CORE_INCLUDE_DIRS})
  endif()

  ADD_PRECOMPILED_HEADER(safir_generated-${_gen_NAME}-cpp
    ${precompiled_header_path}/precompiled_header_for_cpp.h)

  #include path for precompiled_header_for_cpp.h
  target_include_directories(safir_generated-${_gen_NAME}-cpp
    PRIVATE ${precompiled_header_path})

  target_link_libraries(safir_generated-${_gen_NAME}-cpp PRIVATE dots_kernel)

  #On Windows external builds autolinking for dots_cpp is used.
  if (NOT (MSVC AND SAFIR_EXTERNAL_BUILD))
    target_link_libraries(safir_generated-${_gen_NAME}-cpp PRIVATE

      PUBLIC
      dots_cpp)
  endif()

  #On Windows external builds autolinking is used except for other targets in the
  #current build tree.
  foreach (DEP ${ALL_DEPENDENCIES})
    if (NOT (MSVC AND SAFIR_EXTERNAL_BUILD) OR TARGET safir_generated-${DEP}-cpp)
      TARGET_LINK_LIBRARIES(safir_generated-${_gen_NAME}-cpp PRIVATE

        PUBLIC
        safir_generated-${DEP}-cpp)
    endif()
  endforeach()

  add_dependencies(safir_generated-${_gen_NAME}-cpp safir_generated-${_gen_NAME}-code)

  #put the include files in a target property
  set_property(TARGET ${_gen_NAME}-dou PROPERTY
    CXX_INCLUDE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/gen/cpp/include/)

  ############

  #
  # Build Java
  #
  if (Java_FOUND AND NOT ADD_SAFIR_GENERATED_LIBRARY_NO_JAVA)

    if (SAFIR_EXTERNAL_BUILD)
      set (include_jars ${SAFIR_SDK_CORE_JAVA_DIR}/dots_java.jar)
    else()
      set (include_jars dots_java)
    endif()

    FOREACH (DEP ${ALL_DEPENDENCIES})
      if (SAFIR_EXTERNAL_BUILD AND NOT TARGET safir_generated-${DEP}-java)
        FOREACH(inc ${CMAKE_JAVA_INCLUDE_PATH})
          GET_FILENAME_COMPONENT(inc_name ${inc} NAME)
          if ("${inc_name}" STREQUAL "safir_generated-${DEP}-java.jar")
            set(safir_generated-${DEP}-java_FOUND True)
          endif()
        ENDFOREACH()
        if(NOT safir_generated-${DEP}-java_FOUND)
          list(APPEND include_jars ${SAFIR_SDK_CORE_JAVA_DIR}/safir_generated-${DEP}-java.jar)
        endif()
      else()
        list(APPEND include_jars safir_generated-${DEP}-java)
      endif()

      #This gets put into the manifest file by configure_file below
      set(SAFIR_GENERATED_JAVA_MANIFEST_CLASSPATH
        "${SAFIR_GENERATED_JAVA_MANIFEST_CLASSPATH} safir_generated-${DEP}-java.jar")
    ENDFOREACH()

    if (SAFIR_EXTERNAL_BUILD)
      set (manifest_path ${SAFIR_SDK_CORE_GENERATION_DIR}/java/Manifest.txt.in)
    else()
      set (manifest_path ${safir-sdk-core_SOURCE_DIR}/src/dots/dots_v.ss/data/Manifest.txt.in)
    endif()

    configure_file(${manifest_path} ${CMAKE_CURRENT_BINARY_DIR}/Manifest.generated.txt @ONLY)

    ADD_JAR(safir_generated-${_gen_NAME}-java
      SOURCES ${java_files}
      INCLUDE_JARS ${include_jars}
      MANIFEST ${CMAKE_CURRENT_BINARY_DIR}/Manifest.generated.txt)

    add_dependencies(safir_generated-${_gen_NAME}-java safir_generated-${_gen_NAME}-code)

    #remember that we built java
    set_target_properties(${_gen_NAME}-dou PROPERTIES
      JAVA_BUILT True)
  endif()

  ############

  #
  # Build Dotnet
  #
  if (CSHARP_FOUND AND NOT ADD_SAFIR_GENERATED_LIBRARY_NO_DOTNET)

    set (assembly_refs Safir.Dob.Typesystem)
    foreach (DEP ${ALL_DEPENDENCIES})
      list(APPEND assembly_refs safir_generated-${DEP}-dotnet)
    endforeach()

    if (SAFIR_EXTERNAL_BUILD)
      set (snk_path ${SAFIR_SDK_CORE_GENERATION_DIR}/dotnet/dots_generated-dotnet.snk)
      set (lib_path_arg LIBRARY_PATHS ${SAFIR_SDK_CORE_CSHARP_LIBRARY_PATH})
    else()
      set (snk_path ${safir-sdk-core_SOURCE_DIR}/src/dots/dots_v.ss/data/dots_generated-dotnet.snk)
    endif()

    ADD_CSHARP_ASSEMBLY(safir_generated-${_gen_NAME}-dotnet LIBRARY
      SIGN ${snk_path}
      SOURCES ${dotnet_files}
      REFERENCES ${assembly_refs}
      ${lib_path_arg})

    add_dependencies(safir_generated-${_gen_NAME}-dotnet safir_generated-${_gen_NAME}-code)

    #remember that we built dotnet
    set_target_properties(${_gen_NAME}-dou PROPERTIES
      DOTNET_BUILT True)

  endif()

  ############

  #
  # Remember paths to all generated libraries, for use by tests
  # use get_property to get hold of the value (like below)
  #
  get_property(SAFIR_GENERATED_PATHS GLOBAL PROPERTY SAFIR_GENERATED_PATHS)

  set_property(GLOBAL PROPERTY SAFIR_GENERATED_PATHS
    ${SAFIR_GENERATED_PATHS} "SAFIR_GENERATED_${_gen_NAME}_DIR=$<TARGET_FILE_DIR:safir_generated-${_gen_NAME}-cpp>")
  ##############
ENDFUNCTION()

# Documentation for this function can be found in SafirSDKCoreConfig.cmake
FUNCTION(INSTALL_SAFIR_GENERATED_LIBRARY)
  # Work out if we're building the Safir SDK Core source tree or not
  SAFIR_IS_EXTERNAL_BUILD()

  if (SAFIR_EXTERNAL_BUILD)
    cmake_parse_arguments(_in "" "CXX_RUNTIME;CXX_LIBRARY;CXX_ARCHIVE;CXX_INCLUDE;JAR;DOTNET;DOU_BASE" "TARGETS" ${ARGN})
  else()
    cmake_parse_arguments(_in "TEST_SUITE" "" "TARGETS" ${ARGN})
  endif()

  if (NOT "${_in_UNPARSED_ARGUMENTS}" STREQUAL "")
    message(FATAL_ERROR "Unknown argument to INSTALL_SAFIR_GENERATED_LIBRARY '${_in_UNPARSED_ARGUMENTS}'")
  endif()

  foreach (_in_NAME IN LISTS _in_TARGETS)
    if(NOT TARGET ${_in_NAME}-dou)
      message(FATAL_ERROR "No such safir_generated target: ${_in_NAME}")
    endif()

    get_target_property(_in_SOURCE_FILES ${_in_NAME}-dou SOURCE_FILES)
    get_target_property(_in_CXX_INCLUDE_DIRECTORY ${_in_NAME}-dou CXX_INCLUDE_DIRECTORY)
    get_target_property(_in_JAVA_BUILT ${_in_NAME}-dou JAVA_BUILT)
    get_target_property(_in_DOTNET_BUILT ${_in_NAME}-dou DOTNET_BUILT)

    if (SAFIR_EXTERNAL_BUILD)
      if (_in_CXX_RUNTIME AND _in_CXX_LIBRARY)
        if (NOT _in_CXX_ARCHIVE)
          set(_in_CXX_ARCHIVE ${_in_CXX_LIBRARY})
        endif()
        INSTALL(TARGETS safir_generated-${_in_NAME}-cpp
          RUNTIME DESTINATION ${_in_CXX_RUNTIME}
          LIBRARY DESTINATION ${_in_CXX_LIBRARY}
          ARCHIVE DESTINATION ${_in_CXX_ARCHIVE})
      endif()

      if (_in_CXX_INCLUDE)
        INSTALL(DIRECTORY ${_in_CXX_INCLUDE_DIRECTORY}
          DESTINATION ${_in_CXX_INCLUDE}
          PATTERN ".svn" EXCLUDE
          PATTERN "*~" EXCLUDE)
      endif()

      if (_in_DOU_BASE)
        INSTALL(FILES ${_in_SOURCE_FILES}
          DESTINATION ${_in_DOU_BASE}/${_in_NAME})
      endif()

      if (_in_JAVA_BUILT)
        get_target_property(install_files safir_generated-${_in_NAME}-java INSTALL_FILES)

        if (install_files AND _in_JAR)
          INSTALL(FILES ${install_files}
            DESTINATION ${_in_JAR})
        endif()
      endif()

      if (_in_DOTNET_BUILT AND _in_DOTNET)
        INSTALL_CSHARP_ASSEMBLY(TARGET safir_generated-${_in_NAME}-dotnet
          DESTINATION ${_in_DOTNET})
      endif()

    else()
      #For installs that happen from within the Safir SDK Core build tree we use
      #a lot of component stuff etc.
      if (_in_TEST_SUITE)
        set (component_runtime TestSuite)
        set (component_development TestSuite)
        unset(_in_export)
      else()
        set (component_runtime Runtime)
        set (component_development Development)
        set (_in_export EXPORT SafirSDKCore)
      endif()

      INSTALL(TARGETS safir_generated-${_in_NAME}-cpp
        ${_in_export}
        RUNTIME DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN} COMPONENT ${component_runtime}
        LIBRARY DESTINATION ${SAFIR_INSTALL_DESTINATION_LIB} COMPONENT ${component_runtime}
        ARCHIVE DESTINATION ${SAFIR_INSTALL_DESTINATION_LIB} COMPONENT ${component_development})

      INSTALL(DIRECTORY ${_in_CXX_INCLUDE_DIRECTORY}
        DESTINATION ${SAFIR_INSTALL_DESTINATION_INCLUDE}
        COMPONENT ${component_development}
        PATTERN ".svn" EXCLUDE
        PATTERN "*~" EXCLUDE)

      INSTALL(FILES ${_in_SOURCE_FILES}
        DESTINATION ${SAFIR_INSTALL_DESTINATION_DOU_BASE}/${_in_NAME}
        COMPONENT ${component_runtime})

      if (_in_JAVA_BUILT)
        get_target_property(install_files safir_generated-${_in_NAME}-java INSTALL_FILES)

        if (install_files)
          INSTALL(FILES ${install_files}
            DESTINATION ${SAFIR_INSTALL_DESTINATION_JAR}
            COMPONENT ${component_runtime})
        endif()
      endif()

      if (_in_DOTNET_BUILT)
        INSTALL_CSHARP_ASSEMBLY(TARGET safir_generated-${_in_NAME}-dotnet
          DESTINATION ${SAFIR_INSTALL_DESTINATION_CSHARP}
          COMPONENT ${component_runtime})
      endif()
    endif()
  endforeach()

ENDFUNCTION()

#
# Work out if we're building the Safir SDK Core source tree or if
# we're building a user library (i.e. external to Core).
#
# Will set SAFIR_EXTERNAL_BUILD to True or False.
#
# This is really an internal function, and should not be used by anyone
# outside of BuildSafirGenerated.cmake.
#
FUNCTION(SAFIR_IS_EXTERNAL_BUILD)
  if (SAFIR_SDK_CORE_INSTALL_DIR AND safir-sdk-core_SOURCE_DIR)
    MESSAGE(FATAL_ERROR "Please do not use find_package(SafirSDKCore) from "
      "within the Safir SDK Core build tree! What are you trying to do?!")
  elseif(SAFIR_SDK_CORE_INSTALL_DIR)
    set (SAFIR_EXTERNAL_BUILD True PARENT_SCOPE)
  elseif(safir-sdk-core_SOURCE_DIR)
    set (SAFIR_EXTERNAL_BUILD False PARENT_SCOPE)
  else()
    MESSAGE(FATAL_ERROR "Could not work out whether this is an external or "
      "internal build. Did you follow the instructions in the users guide?")
  endif()
ENDFUNCTION()
