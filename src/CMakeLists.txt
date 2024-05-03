include(SafirCompilerSettings)
include(SafirLoadBoost)

if (MSVC)
  SET(CMAKE_DEBUG_POSTFIX "d")
endif()


#
# Find an ODBC installation, of either iODBC or UnixODBC
#
FIND_PACKAGE(ODBC)
###########


#
# Load Qt6, (or Qt5 on Windows if Qt6 is not available)
#

# As moc files are generated in the binary dir, tell CMake
# to always look for includes there:
set(CMAKE_INCLUDE_CURRENT_DIR ON)

# Conan does not generate the versionless targets, so we
# need to do that ourselves. First we disable them for when
# do not run using conan.
set(QT_NO_CREATE_VERSIONLESS_TARGETS True)

find_package(Qt6 COMPONENTS Core Widgets QUIET)

if (MSVC)
  # On Windows we build qt in Conan and link statically
  #we also look for qt5 if qt6 was not available.
  if (Qt6_FOUND)
    if (TARGET Qt6::QModernWindowsStylePlugin)
      set (windows_plugin Qt6::QModernWindowsStylePlugin)
    elseif (TARGET Qt6::QWindowsVistaStylePlugin)
      set (windows_plugin Qt6::QWindowsVistaStylePlugin)
    else ()
      message(FATAL_ERROR "Could not find the appropriate windows style plugin for Qt, please investigate!")
    endif()

    target_link_libraries(Qt6::Widgets
      INTERFACE
      Qt6::QWindowsIntegrationPlugin
      ${windows_plugin}
      #These are windows libraries that qt needs to link against.
      shcore
      UxTheme
      Setupapi
      Dcomp
      D3d12)
    unset(windows_plugin)
  else()
    find_package(Qt5 COMPONENTS Core Widgets QUIET)
    if (Qt5_FOUND)
      # On Windows we build qt in Conan and link statically
      target_link_libraries(Qt5::Widgets
        INTERFACE
        Qt5::QWindowsIntegrationPlugin
        Qt5::QWindowsVistaStylePlugin
        debug Qt5FontDatabaseSupportd
        debug Qt5EventDispatcherSupportd
        debug Qt5ThemeSupportd
        debug Qt5WindowsUIAutomationSupportd
        optimized Qt5FontDatabaseSupport
        optimized Qt5EventDispatcherSupport
        optimized Qt5ThemeSupport
        optimized Qt5WindowsUIAutomationSupport)
    endif()
  endif()

  add_definitions(-DSAFIR_LINK_QT_STATICALLY)
endif()

# And then we need to define the versionless targets
if (Qt6_FOUND)
  add_library (Qt::Core ALIAS Qt6::Core)
  add_library (Qt::Widgets ALIAS Qt6::Widgets)
elseif(Qt5_FOUND)
  add_library (Qt::Core ALIAS Qt5::Core)
  add_library (Qt::Widgets ALIAS Qt5::Widgets)
else()
  message(WARNING "Appropriate version of Qt was not found. Will not build tools that depend on Qt.")
endif()

#
# Install a target to the proper place. This function tries to put all parts
# of the targets into the correct places.
#
# Usage: SAFIR_INSTALL(TARGETS <target1> .. <targetN>
#                      [JNI]
#                      [COMPONENT <component>])
#
# JNI: Use flag to specify that this is a Java Native Interface library.
#      This will cause namelink installation to be handled slightly differently.
# COMPONENT: Specify which component the target belongs to. Can be Runtime,
#            Tools, Development or TestSuite. Is set to Runtime if not specified.
#
function (SAFIR_INSTALL)
  cmake_parse_arguments(INST "DEVELOPMENT;TEST_SUITE;JNI" "COMPONENT" "TARGETS" ${ARGN})

  if (NOT "${INST_UNPARSED_ARGUMENTS}" STREQUAL "")
    message(FATAL_ERROR "Unknown argument to SAFIR_INSTALL '${INST_UNPARSED_ARGUMENTS}'")
  endif()

  if (INST_DEVELOPMENT OR INST_TEST_SUITE)
    message(FATAL_ERROR "SAFIR_INSTALL no longer uses DEVELOPMENT or TEST_SUITE targets, use COMPONENT instead")
  endif()

  #check component and use Runtime if it was not specified explicitly
  if (NOT INST_COMPONENT)
    set(INST_COMPONENT Runtime)
  endif()
  set (_components Runtime Tools Development TestSuite)
  list(FIND _components "${INST_COMPONENT}" _valid_component)
  if(_valid_component EQUAL -1)
    message(FATAL_ERROR "Invalid COMPONENT '${INST_COMPONENT}'")
  endif()

  foreach (target ${INST_TARGETS})
    get_target_property(target_type ${target} TYPE)
    get_target_property(assembly_file ${target} ASSEMBLY_FILE)
    get_target_property(jar_file ${target} JAR_FILE)
    ################ Dotnet ####################
    if (assembly_file)
      #by default assemblies get installed here
      set(destination ${SAFIR_INSTALL_DESTINATION_CSHARP})

      #except exes on windows
      get_property(ASSEMBLY_KIND TARGET ${target} PROPERTY TARGET_KIND)
      if (WIN32 AND NOT ASSEMBLY_KIND STREQUAL "library")
        set(destination ${SAFIR_INSTALL_DESTINATION_BIN})
      endif()

      INSTALL_CSHARP_ASSEMBLY(TARGET ${target}
        DESTINATION ${destination}
        COMPONENT ${INST_COMPONENT})

      #and on unix systems we want a little shell script in bin to launch exes with
      if (UNIX AND NOT ASSEMBLY_KIND STREQUAL "library")
        #we need the assembly file name, so we get the full path first
        get_property(assembly_file TARGET ${target} PROPERTY ASSEMBLY_FILE)
        get_filename_component(assembly_file ${assembly_file} NAME) #and strip the path off
        if (ASSEMBLY_KIND STREQUAL "winexe")
          set(winforms_workaround "--verify-all")
        endif()
        find_program(MONO_EXECUTABLE NAMES mono)
        if (NOT MONO_EXECUTABLE)
          message(FATAL_ERROR "Mono executable not found!")
        endif()
        FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/${target} "#!/bin/sh\nexec ${MONO_EXECUTABLE} ${winforms_workaround} $MONO_OPTIONS ${CMAKE_INSTALL_PREFIX}/${SAFIR_INSTALL_DESTINATION_CSHARP}/${assembly_file} $@")
        #and install it to bin directory
        install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${target}
          DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN}
          COMPONENT ${INST_COMPONENT})
      endif()

    elseif (jar_file)
      #################### Java ########################
      get_target_property(install_files ${target} INSTALL_FILES)

      if (install_files)
        INSTALL(FILES ${install_files}
          DESTINATION ${SAFIR_INSTALL_DESTINATION_JAR}
          COMPONENT ${INST_COMPONENT})
      endif()

    else()
      ############### C/C++ ####################
      if ("${target_type}" STREQUAL "SHARED_LIBRARY")
        if (INST_DEVELOPMENT)
          message(FATAL_ERROR "SAFIR_INSTALL does currently not support the DEVELOPMENT flag for library targets. Add support, if it makes sense.")
        endif()
        if (INST_COMPONENT STREQUAL "TestSuite")
          unset(INST_export)
        else()
          set(INST_export EXPORT SafirSDKCore)
        endif()

      else()
        unset(INST_export)
      endif()

      #Runtime and Tools components need special treatment.
      if (INST_COMPONENT STREQUAL "Runtime" OR INST_COMPONENT STREQUAL "Tools")
        INSTALL(TARGETS ${target}
          ${INST_export}
          RUNTIME DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN} COMPONENT ${INST_COMPONENT}
          LIBRARY DESTINATION ${SAFIR_INSTALL_DESTINATION_LIB} COMPONENT ${INST_COMPONENT} NAMELINK_SKIP
          ARCHIVE DESTINATION ${SAFIR_INSTALL_DESTINATION_LIB} COMPONENT Development)

        #JNI libraries need to have their namelink installed as part of runtime instead of development.
        if (INST_JNI)
          INSTALL(TARGETS ${target}
            LIBRARY DESTINATION ${SAFIR_INSTALL_DESTINATION_LIB} COMPONENT ${INST_COMPONENT} NAMELINK_ONLY)
        elseif ("${target_type}" STREQUAL "SHARED_LIBRARY")
          INSTALL(TARGETS ${target}
            LIBRARY DESTINATION ${SAFIR_INSTALL_DESTINATION_LIB} COMPONENT Development NAMELINK_ONLY)
        endif()
      else()
        INSTALL(TARGETS ${target}
          ${INST_export}
          RUNTIME DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN} COMPONENT ${INST_COMPONENT}
          LIBRARY DESTINATION ${SAFIR_INSTALL_DESTINATION_LIB} COMPONENT ${INST_COMPONENT}
          ARCHIVE DESTINATION ${SAFIR_INSTALL_DESTINATION_LIB} COMPONENT ${INST_COMPONENT})
      endif()

      # Install pdb files on windows
      if(MSVC)
        if (INST_COMPONENT STREQUAL "TestSuite")
          INSTALL(FILES $<TARGET_PDB_FILE:${target}>
            DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN}
            COMPONENT TestSuite)
        else()
          INSTALL(FILES $<TARGET_PDB_FILE:${target}>
            DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN}
            COMPONENT Development)
        endif()
      endif()
    endif()
  endforeach()
endfunction()
###########

if (UNIX)
  set(PATH_SEPARATOR ":")
else()
  set(PATH_SEPARATOR ";")
endif()

#
# This is an "overload" of the builtin ADD_LIBRARY function. The
# purpose is threefold:
#
# 1. Set the version properties (VERSION and SOVERSION) on the targets, to make *nix
#    builds get correct library naming.
# 2. Collect the paths of all libraries so that it will be possible to link
#    the header_check target.
# 3. Collect the paths of all libraries so that SET_SAFIR_TEST_PROPERTIES can add them
#    to PATH/LD_LIBRARY_PATH. Without this lots of tests can't execute.
#
# It calls the builtin ADD_LIBRARY routine when the other tasks have been performed.
#
function(add_library NAME)
  list(FIND ARGN "IMPORTED" is_imported)
  list(FIND ARGN "INTERFACE" is_interface)
  if(NOT is_imported EQUAL -1 OR NOT is_interface EQUAL -1)
    #call the real add_library function
    _add_library(${NAME} ${ARGN})

    #and get out of here, the rest of this function is only for our own targets
    return()
  endif()

  #call the real add_library function
  _add_library(${NAME} ${ARGN})

  #Set library versions
  set_property(TARGET ${NAME} PROPERTY VERSION ${SAFIR_VERSION_STRING})
  set_property(TARGET ${NAME} PROPERTY SOVERSION ${SAFIR_VERSION_MAJOR})
endfunction()

#
# Set up common properties on tests
#
# Note that it is most likely not possible to combine this function with any of the
# other CMake ways of setting properties on tests. It appears that they override each other.
#
# Usage: SET_SAFIR_TEST_PROPERTIES(TEST <test_target>
#                                  [TIMEOUT <timeout>]
#                                  [CONFIG_OVERRIDE <config_dir>]
#                                  [ENVIRONMENT ...])
#
# TIMEOUT: Time in seconds after which the test is interrupted.
#          If this option is not specified the timeout will be set to 120s by default.
# CONFIG_OVERRIDE: Absolute path to directory containing safir configuration. If not specified
#                  it will be set to a default configuration.
# ENVIRONMENT: Specify more environment variables to be passed to the test.
#
function (SET_SAFIR_TEST_PROPERTIES)
  cmake_parse_arguments(PROP "" "TEST;TIMEOUT;CONFIG_OVERRIDE" "ENVIRONMENT" ${ARGN})

  if (NOT "${PROP_UNPARSED_ARGUMENTS}" STREQUAL "")
    message(FATAL_ERROR "Unknown argument to SET_SAFIR_TEST_PROPERTIES '${PROP_UNPARSED_ARGUMENTS}'")
  endif()

  if (NOT PROP_TEST)
    message(FATAL_ERROR "SET_SAFIR_TEST_PROPERTIES needs TEST argument")
  endif()


  if (PROP_TIMEOUT)
    SET_TESTS_PROPERTIES(${PROP_TEST} PROPERTIES TIMEOUT ${PROP_TIMEOUT})
  else()
    SET_TESTS_PROPERTIES(${PROP_TEST} PROPERTIES TIMEOUT 120)
  endif()

  if (NOT PROP_CONFIG_OVERRIDE)
    set(PROP_CONFIG_OVERRIDE "${safir-sdk-core_SOURCE_DIR}/src/tests/test_support/test_config")

    #Create the directory that our default logging.ini will put its logs in.
    make_directory(${CMAKE_CURRENT_BINARY_DIR}/test_output/${PROP_TEST}/)
  endif()

  set (pypath "$ENV{PYTHONPATH}${PATH_SEPARATOR}${safir-sdk-core_SOURCE_DIR}/src/tests/test_support/python")
  string(REGEX REPLACE "^${PATH_SEPARATOR}+" "" pypath ${pypath}) # remove any leading path separators

  if (MSVC)
    if (CMAKE_CONFIGURATION_TYPES)
      foreach(_config_type IN LISTS CMAKE_CONFIGURATION_TYPES)
        set(_extra_path "${_extra_path}$<$<CONFIG:${_config_type}>:${CMAKE_RUNTIME_OUTPUT_DIRECTORY_${_config_type}}>")
      endforeach()
      set(PATH_ENVIRONMENT "PATH=$ENV{PATH};${_extra_path}")
    else()
      set(PATH_ENVIRONMENT "PATH=$ENV{PATH};${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")
    endif()
    STRING(REPLACE "\;" ";" PATH_ENVIRONMENT "${PATH_ENVIRONMENT}")
    STRING(REPLACE ";" "\\;" PATH_ENVIRONMENT "${PATH_ENVIRONMENT}")
  else()
    set(PATH_ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_LIBRARY_OUTPUT_DIRECTORY}")
  endif()

  # Set a variable that points to where put all the generated libraries for use by tests
  if (MSVC)
    if (CMAKE_CONFIGURATION_TYPES)
      set(SAFIR_GENERATED_LIB_OUTPUT_DIRECTORY ${_extra_path})
    else()
      set(SAFIR_GENERATED_LIB_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
    endif()
  else()
    set(SAFIR_GENERATED_LIB_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})
  endif()

  SET_PROPERTY(TEST ${PROP_TEST}
    PROPERTY ENVIRONMENT
    "SAFIR_TEST_CONFIG_OVERRIDE=${PROP_CONFIG_OVERRIDE}"
    "SAFIR_SOURCE_ROOT=${safir-sdk-core_SOURCE_DIR}/"
    "CMAKE_CURRENT_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}"
    "CMAKE_CURRENT_BINARY_DIR=${CMAKE_CURRENT_BINARY_DIR}"
    "CMAKE_RUNTIME_OUTPUT_DIRECTORY=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
    "CMAKE_CSHARP_OUTPUT_DIRECTORY=${CMAKE_CSHARP_OUTPUT_DIRECTORY}"
    "CMAKE_JAVA_OUTPUT_DIRECTORY=${CMAKE_JAVA_OUTPUT_DIRECTORY}"
    "SAFIR_GENERATED_LIB_OUTPUT_DIRECTORY=${SAFIR_GENERATED_LIB_OUTPUT_DIRECTORY}"
    "SAFIR_TEST_NAME=${PROP_TEST}"
    "PYTHONPATH=${pypath}"
    "${PATH_ENVIRONMENT}"
    ${PROP_ENVIRONMENT})
endfunction()
################

#
# This is an "overload" of the builtin TARGET_LINK_LIBRARIES function.
#
# The purpose is to check that the keyword PRIVATE has not been left out.
# This check is slightly overzealous, since not all TARGET_LINK_LIBRARIES
# calls actually need to use the PRIVATE keyword, but those few exceptions
# can just add it anyway.
#
function(target_link_libraries)
  list(FIND ARGN "PRIVATE" has_private_keyword)
  if(has_private_keyword EQUAL -1)
    message(FATAL_ERROR "target_link_libraries lacks PRIVATE keyword!")
  endif()
  _target_link_libraries(${ARGN})
endfunction()
##############

INCLUDE(BuildSafirGenerated)

include_directories(lluf/lluf_build_helpers.ss/src)

ADD_DEFINITIONS(-DSAFIR_NO_AUTOLINK)


#
# Set up .NET compilation
#
include(SafirLoadCSharp)

#
# Set up Java compilation
#
include(SafirLoadJava)


#
# Set up Python
#
FIND_PACKAGE(Python3 COMPONENTS Interpreter)
if(Python3_VERSION VERSION_LESS "3.2")
  message(FATAL_ERROR "Need Python version 3.2 or greater. Sorry.")
endif()

#
# Set up Protobuf
#
FIND_PACKAGE(Protobuf REQUIRED)
include(protobuf-generate)
include(protobuf-module)

#
# Define a function that adds a subdirectory and also sets the name
# as the IDE folder name
#

function (add_subdirectory_and_set_ide_folder NAME)
  set(CMAKE_FOLDER ${NAME})
  add_subdirectory(${NAME})
endfunction()

#
# At last, we're ready to add our subdirectories.
#

add_subdirectory_and_set_ide_folder(patch_check)
add_subdirectory_and_set_ide_folder(lluf)
add_subdirectory_and_set_ide_folder(logging)
add_subdirectory_and_set_ide_folder(distribution)
add_subdirectory_and_set_ide_folder(dots)
add_subdirectory_and_set_ide_folder(safir_dou)
add_subdirectory_and_set_ide_folder(control)
add_subdirectory_and_set_ide_folder(dose)
add_subdirectory_and_set_ide_folder(status)
add_subdirectory_and_set_ide_folder(douf)
add_subdirectory_and_set_ide_folder(swre)
add_subdirectory_and_set_ide_folder(foreach)
add_subdirectory_and_set_ide_folder(dope)
add_subdirectory_and_set_ide_folder(websocket)
add_subdirectory_and_set_ide_folder(tests)
add_subdirectory_and_set_ide_folder(util)
add_subdirectory_and_set_ide_folder(config)


#non-msvc systems need the cmake export file.
#msvc uses autolinking, so we don't need this file there.
if (NOT MSVC)
  INSTALL(EXPORT SafirSDKCore DESTINATION ${SAFIR_INSTALL_DESTINATION_CMAKE}
    COMPONENT Development)
endif()

configure_file(cmake/SafirSDKCoreConfig.cmake.in
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SafirSDKCoreConfig.cmake" @ONLY)

INSTALL(FILES
  cmake/BuildSafirGenerated.cmake
  cmake/CSharpMacros.cmake
  cmake/SafirCompilerSettings.cmake
  cmake/SafirLoadCSharp.cmake
  cmake/FindCSharp.cmake
  cmake/SafirLoadJava.cmake
  cmake/PrecompiledHeader.cmake
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SafirSDKCoreConfig.cmake"
  DESTINATION ${SAFIR_INSTALL_DESTINATION_CMAKE}
  COMPONENT Development)
