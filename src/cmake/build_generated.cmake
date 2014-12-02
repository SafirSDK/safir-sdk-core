include(CMakeParseArguments)

#
# Work out if we're building the Safir SDK Core source tree or if
# we're building a user library (i.e. external to Core).
#
# Will set SAFIR_EXTERNAL_BUILD to True or False.
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


FUNCTION(ADD_SAFIR_GENERATED_LIBRARY)
  cmake_parse_arguments(GEN "" "NAME" "DEPENDENCIES" ${ARGN})

  if ("${GEN_NAME}" STREQUAL "")
    message(FATAL_ERROR "Invalid NAME passed to ADD_SAFIR_GENERATED_LIBRARY")
  endif()

  if (NOT "${GEN_UNPARSED_ARGUMENTS}" STREQUAL "")
    message(FATAL_ERROR "Unknown argument to BUILD_GENERATED_LIBRARY '${GEN_UNPARSED_ARGUMENTS}'")
  endif()

  # Work out if we're building the Safir SDK Core source tree or not
  SAFIR_IS_EXTERNAL_BUILD()

  if (SAFIR_EXTERNAL_BUILD)
    #load compiler settings, csharp and java!
    include(${SAFIR_SDK_CORE_CMAKE_DIR}/SafirCompilerSettings.cmake)
    include(${SAFIR_SDK_CORE_DOTNET_SETTINGS})
    include(${SAFIR_SDK_CORE_JAVA_SETTINGS})

    #We need boost headers.
    set(Boost_FIND_QUIETLY True)
    find_package(Boost COMPONENTS system thread REQUIRED)
    include_directories(${Boost_INCLUDE_DIRS})

    #dont use autolinking with boost
    ADD_DEFINITIONS(-DBOOST_ALL_NO_LIB)

    #use dynamic linking with boost
    ADD_DEFINITIONS(-DBOOST_ALL_DYN_LINK)

    #Debug dlls on windows need d suffix
    if (MSVC)
      SET(CMAKE_DEBUG_POSTFIX "d")
    endif()
  endif()

  #
  # Dependency resolution
  #
  # Create custom targets dummy that we just use to have somewhere to put the dependencies.
  # Then, when we need to resolve a targets dependencies, we recursively look at the dummy
  # targets to find all of the dependencies of our dependencies.
  add_custom_target(${GEN_NAME}-dou)
  set_target_properties(${GEN_NAME}-dou PROPERTIES
    DOU_DEPENDENCIES "${GEN_DEPENDENCIES}"
    DOU_DIR ${CMAKE_CURRENT_SOURCE_DIR})

  #recursively get all dependendencies
  function(GET_DEPS)
    cmake_parse_arguments(DEP "" "" "DEPENDENCIES" ${ARGN})

    FOREACH (DEP ${DEP_DEPENDENCIES})
      if (NOT "${DEP}" STREQUAL "")
        if (TARGET ${DEP}-dou)
          get_target_property(deps ${DEP}-dou DOU_DEPENDENCIES)
          if ("${deps}" STREQUAL "deps-NOTFOUND")
            MESSAGE(FATAL_ERROR "Could not resolve dependency '${DEP}'")
          else()
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

  GET_DEPS(DEPENDENCIES ${GEN_DEPENDENCIES})
  set(ALL_DEPENDENCIES ${GET_DEPS_RESULT})
  ################


  #
  # Set up variables containing all dou files and all expected source code files
  #
  FILE(GLOB_RECURSE dou_files *.dou)
  FILE(GLOB_RECURSE dom_files *.dom)
  FILE(GLOB_RECURSE namespace_files *.namespace.txt)

  #put the files in a target property so the INSTALL_SAFIR_GENERATED_LIBRARY
  #function can know what files it needs to install
  set_property(TARGET ${GEN_NAME}-dou PROPERTY
    SOURCE_FILES ${dou_files} ${dom_files} ${namespace_files})

  #set up java namespace prefixing rules
  foreach (file ${namespace_files})
    string (REGEX REPLACE ".*/([a-zA-Z\\.0-9]*)-java\\.namespace\\.txt" "\\1" namespace ${file})

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
  foreach (dou ${dou_files})
    string (REGEX REPLACE ".*/([a-zA-Z\\.0-9]*)\\.dou" "\\1" base_name ${dou})
    set (cpp_files ${cpp_files} generated_code/cpp/${base_name}.cpp)
    set (dotnet_files ${dotnet_files} "${CMAKE_CURRENT_BINARY_DIR}/generated_code/dotnet/${base_name}.cs")


    string (REGEX REPLACE "^([a-zA-Z\\.0-9]*)\\.[a-zA-Z0-9]+$" "\\1" namespace ${base_name})
    string (REGEX REPLACE "^[a-zA-Z\\.0-9]*\\.([a-zA-Z0-9]+)$" "\\1" java_base_name ${base_name})

    #  TODO: we need to read namespace files from our dependencies too!
    #or maybe warn when there is a difference? No, thats no good....

    #perform prefix insertion
    foreach(key ${java_namespace_keys})
      string (REGEX REPLACE "^${key}" "${java_namespace_${key}_replacement}" namespace ${namespace})
    endforeach()


    string (TOLOWER ${namespace} java_namespace)
    string (REPLACE "." "/" java_path ${java_namespace})
    set (java_files ${java_files}
      "${CMAKE_CURRENT_BINARY_DIR}/generated_code/java/src/${java_path}/${java_base_name}.java")
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

  FILE(GLOB dod_files ${dod_directory} *.dod)

  SET(dots_v_command ${PYTHON_EXECUTABLE}
    ${dots_v_path}
    --dod-files=${dod_directory}
    --dependencies ${DOTS_V_DEPS}
    --library-name ${GEN_NAME}
    --output-path=generated_code)

  ADD_CUSTOM_COMMAND(
    OUTPUT ${cpp_files} ${java_files} ${dotnet_files}

    COMMAND ${dots_v_command} ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS ${dod_files} ${dou_files} ${namespace_files}
    COMMENT "Generating code for ${CMAKE_CURRENT_SOURCE_DIR}")

  #make clean target remove the generated_code directory
  SET_DIRECTORY_PROPERTIES(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES generated_code)

  #We need a custom target that the library (java,cpp,dotnet) targets can depend on, since
  #having them all just depend on the output files will wreak havoc with cmake in parallel builds.
  #See http://public.kitware.com/Bug/view.php?id=12311
  add_custom_target(safir_generated-${GEN_NAME}-code ALL DEPENDS ${cpp_files} ${java_files} ${dotnet_files})
  #############

  #
  # Build CPP
  #
  ADD_LIBRARY(safir_generated-${GEN_NAME}-cpp SHARED ${cpp_files})

  target_include_directories(safir_generated-${GEN_NAME}-cpp
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/generated_code/cpp/include>)

  #include path for precompiled_header_for_cpp.h
  if (SAFIR_EXTERNAL_BUILD)
    target_include_directories(safir_generated-${GEN_NAME}-cpp
      PRIVATE
      ${SAFIR_SDK_CORE_GENERATION_DIR}/cpp
      ${SAFIR_SDK_CORE_INCLUDE_DIRS})
  else()
    target_include_directories(safir_generated-${GEN_NAME}-cpp
      PRIVATE ${safir-sdk-core_SOURCE_DIR}/src/dots/dots_v.ss/data)
  endif()


  target_link_libraries(safir_generated-${GEN_NAME}-cpp
    dots_cpp
    dots_internal
    dots_kernel
    lluf_utils
    ${Boost_THREAD_LIBRARY}
    ${Boost_SYSTEM_LIBRARY})

  FOREACH (DEP ${ALL_DEPENDENCIES})
    TARGET_LINK_LIBRARIES(safir_generated-${GEN_NAME}-cpp safir_generated-${DEP}-cpp)
  ENDFOREACH()

  add_dependencies(safir_generated-${GEN_NAME}-cpp safir_generated-${GEN_NAME}-code)

  #put the include files in a target property
  set_property(TARGET ${GEN_NAME}-dou PROPERTY
    CXX_INCLUDE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/generated_code/cpp/include/)

  #TODO:precompiled headers?!

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
      if (SAFIR_EXTERNAL_BUILD)
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

    ADD_JAR(safir_generated-${GEN_NAME}-java
      SOURCES ${java_files}
      INCLUDE_JARS ${include_jars}
      MANIFEST ${CMAKE_CURRENT_BINARY_DIR}/Manifest.generated.txt)

    add_dependencies(safir_generated-${GEN_NAME}-java safir_generated-${GEN_NAME}-code)

    #remember that we built java
    set_target_properties(${GEN_NAME}-dou PROPERTIES
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

    ADD_CSHARP_ASSEMBLY(safir_generated-${GEN_NAME}-dotnet LIBRARY
      SIGN ${snk_path}
      SOURCES ${dotnet_files}
      REFERENCES ${assembly_refs}
      ${lib_path_arg})

    add_dependencies(safir_generated-${GEN_NAME}-dotnet safir_generated-${GEN_NAME}-code)

    #remember that we built dotnet
    set_target_properties(${GEN_NAME}-dou PROPERTIES
      DOTNET_BUILT True)

  endif()

  ############

  #
  # Remember paths to all generated libraries, for use by tests
  # use get_property to get hold of the value (like below)
  #
  get_property(SAFIR_GENERATED_PATHS GLOBAL PROPERTY SAFIR_GENERATED_PATHS)

  set_property(GLOBAL PROPERTY SAFIR_GENERATED_PATHS
    ${SAFIR_GENERATED_PATHS} "SAFIR_GENERATED_${GEN_NAME}_DIR=$<TARGET_FILE_DIR:safir_generated-${GEN_NAME}-cpp>")
  ##############


ENDFUNCTION()


FUNCTION(INSTALL_SAFIR_GENERATED_LIBRARY)
  # Work out if we're building the Safir SDK Core source tree or not
  SAFIR_IS_EXTERNAL_BUILD()

  if (SAFIR_EXTERNAL_BUILD)
    cmake_parse_arguments(_in "" "CXX_BIN;CXX_LIB;CXX_INCLUDE;JAR;DOTNET;DOU_BASE" "TARGETS" ${ARGN})
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
      if (_in_CXX_BIN AND _in_CXX_LIB)
        INSTALL(TARGETS safir_generated-${_in_NAME}-cpp
          RUNTIME DESTINATION ${_in_CXX_BIN}
          LIBRARY DESTINATION ${_in_CXX_LIB}
          ARCHIVE DESTINATION ${_in_CXX_LIB})
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
        set (component_runtime Test)
        set (component_development Test)
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
        if (_in_TEST_SUITE)
          set (TEST_SUITE "TEST_SUITE")
        else()
          unset (TEST_SUITE)
        endif()
        INSTALL_CSHARP_ASSEMBLY(TARGET safir_generated-${_in_NAME}-dotnet
          DESTINATION ${SAFIR_INSTALL_DESTINATION_CSHARP}
          ${TEST_SUITE})
      endif()
    endif()
  endforeach()

ENDFUNCTION()
