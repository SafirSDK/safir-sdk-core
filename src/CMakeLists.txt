include(SafirCompilerSettings)
include(SafirLoadBoost)


if (MSVC)
  SET(CMAKE_DEBUG_POSTFIX "d")
endif()


#
# Find an ODBC installation, of either iODBC or UnixODBC
#
if (UNIX)
    FIND_PACKAGE(IODBC)
    if (IODBC_FOUND)
      INCLUDE_DIRECTORIES(${IODBC_INCLUDE_DIRS})
      SET(SAFIR_ODBC_LIBRARIES ${IODBC_LIBRARIES})
      ADD_DEFINITIONS(-DSAFIR_ODBC_IS_IODBC)
    else()
      message(STATUS "iODBC not found, trying UnixODBC")
      FIND_PACKAGE(UnixODBC)
      if (NOT UnixODBC_FOUND)
        message(FATAL_ERROR "Failed to find either iODBC or UnixODBC!")
      endif()
      INCLUDE_DIRECTORIES(${UnixODBC_INCLUDE_DIRS})
      SET(SAFIR_ODBC_LIBRARIES ${UnixODBC_LIBRARIES})
      ADD_DEFINITIONS(-DSAFIR_ODBC_IS_UNIXODBC)
    endif()
endif()
###########


#
# Load Qt5
#
# As moc files are generated in the binary dir, tell CMake
# to always look for includes there:
set(CMAKE_INCLUDE_CURRENT_DIR ON)
find_package(Qt5Widgets QUIET)


#
# Install a target to the proper place. This function tries to put all parts
# of the targets into the correct places.
#
# Usage: SAFIR_INSTALL(TARGETS <target1> .. <targetN>
#                      [JNI]
#                      [DEVELOPMENT]
#                      [TEST_SUITE])
#
# JNI: Use flag to specify that this is a Java Native Interface library.
#      This will cause namelink installation to be handled slightly differently.
# DEVELOPMENT: Use this flag to specify that this target is part of the
#              Development component.
# TEST_SUITE: Use this flag to specify that this target is part of the
#             Test component
#
function (SAFIR_INSTALL)
  cmake_parse_arguments(INST "TEST_SUITE;DEVELOPMENT;JNI" "" "TARGETS" ${ARGN})

  if (NOT "${INST_UNPARSED_ARGUMENTS}" STREQUAL "")
    message(FATAL_ERROR "Unknown argument to SAFIR_INSTALL '${INST_UNPARSED_ARGUMENTS}'")
  endif()

  if (INST_DEVELOPMENT AND INST_TEST_SUITE)
    message(FATAL_ERROR "You can only specify either TEST_SUITE or DEVELOPMENT!")
  endif()

  foreach (target ${INST_TARGETS})
    get_target_property(target_type ${target} TYPE)
    #UTILITY means that it is a dotnet or java target, probably
    if (target_type STREQUAL "UTILITY")
      ################ Dotnet ####################
      get_target_property(assembly_file ${target} ASSEMBLY_FILE)
      if (assembly_file)
        if (INST_TEST_SUITE)
          set (CS_COMP "TEST_SUITE")
          set (CONFIG_COMPONENT Test)
        elseif(INST_DEVELOPMENT)
          set (CS_COMP "DEVELOPMENT")
          set (CONFIG_COMPONENT Development)
        else()
          unset (CS_COMP)
          set (CONFIG_COMPONENT Runtime)
        endif()

        #by default assemblies get installed here
        set(destination ${SAFIR_INSTALL_DESTINATION_CSHARP})

        #except exes on windows
        get_property(ASSEMBLY_KIND TARGET ${target} PROPERTY TARGET_KIND)
        if (WIN32 AND NOT ASSEMBLY_KIND STREQUAL "library")
          set(destination ${SAFIR_INSTALL_DESTINATION_BIN})
        endif()

        INSTALL_CSHARP_ASSEMBLY(TARGET ${target}
          DESTINATION ${destination}
          ${CS_COMP})

        #and on unix systems we want a little shell script in bin to launch exes with
        if (UNIX AND NOT ASSEMBLY_KIND STREQUAL "library")
          #we need the assembly file name, so we get the full path first
          get_property(assembly_file TARGET ${target} PROPERTY ASSEMBLY_FILE)
          get_filename_component(assembly_file ${assembly_file} NAME) #and strip the path off
          FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/${target} "#!/bin/sh\nexec /usr/bin/mono $MONO_OPTIONS ${CMAKE_INSTALL_PREFIX}/${SAFIR_INSTALL_DESTINATION_CSHARP}/${assembly_file} $@")
          #and install it to bin directory
          install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${target}
            DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN}
            COMPONENT ${CONFIG_COMPONENT})
        endif()
      endif()

      #################### Java ########################
      get_target_property(jar_file ${target} JAR_FILE)
      if (jar_file)
        if (INST_TEST_SUITE)
          set (component Test)
        elseif (INST_DEVELOPMENT)
          set (component Development)
        else()
          set (component Runtime)
        endif()

        get_target_property(install_files ${target} INSTALL_FILES)

        if (install_files)
          INSTALL(FILES ${install_files}
            DESTINATION ${SAFIR_INSTALL_DESTINATION_JAR}
            COMPONENT ${component})
        endif()
      endif()

      if (NOT jar_file AND NOT assembly_file)
        message(FATAL_ERROR "The target ${target} does not appear to be either a java or dotnet target, cannot use SAFIR_INSTALL.")
      endif()
    else() #Not a dotnet or java thing

      ############### C/C++ ####################
      if ("${target_type}" STREQUAL "SHARED_LIBRARY")
        if (INST_DEVELOPMENT)
          message(FATAL_ERROR "SAFIR_INSTALL does currently not support the DEVELOPMENT flag for library targets. Add support, if it makes sense.")
        endif()
        if (INST_TEST_SUITE)
          unset(INST_export)
        else()
          set(INST_export EXPORT SafirSDKCore)
        endif()

        set(debug_suffix ${CMAKE_DEBUG_POSTFIX})
      else()
        unset(INST_export)
        unset(debug_suffix)
      endif()

      #if the target is part of the test suite we use the Test component
      if (INST_TEST_SUITE)
        INSTALL(TARGETS ${target}
          ${INST_export}
          RUNTIME DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN} COMPONENT Test
          LIBRARY DESTINATION ${SAFIR_INSTALL_DESTINATION_LIB} COMPONENT Test
          ARCHIVE DESTINATION ${SAFIR_INSTALL_DESTINATION_LIB} COMPONENT Test)
      elseif (INST_DEVELOPMENT)
        INSTALL(TARGETS ${target}
          ${INST_export}
          RUNTIME DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN} COMPONENT Development
          LIBRARY DESTINATION ${SAFIR_INSTALL_DESTINATION_LIB} COMPONENT Development
          ARCHIVE DESTINATION ${SAFIR_INSTALL_DESTINATION_LIB} COMPONENT Development)
      else()
        INSTALL(TARGETS ${target}
          ${INST_export}
          RUNTIME DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN} COMPONENT Runtime
          LIBRARY DESTINATION ${SAFIR_INSTALL_DESTINATION_LIB} COMPONENT Runtime NAMELINK_SKIP
          ARCHIVE DESTINATION ${SAFIR_INSTALL_DESTINATION_LIB} COMPONENT Development)

        #JNI libraries need to have their namelink installed as part of runtime instead of development.
        if (INST_JNI)
          INSTALL(TARGETS ${target}
            LIBRARY DESTINATION ${SAFIR_INSTALL_DESTINATION_LIB} COMPONENT Runtime NAMELINK_ONLY)
        elseif ("${target_type}" STREQUAL "SHARED_LIBRARY")
          INSTALL(TARGETS ${target}
            LIBRARY DESTINATION ${SAFIR_INSTALL_DESTINATION_LIB} COMPONENT Development NAMELINK_ONLY)
        endif()
      endif()

      # Install pdb files on windows
      if(MSVC)
        if (INST_TEST_SUITE)
          INSTALL(FILES $<TARGET_FILE_DIR:${target}>/${target}.pdb
            DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN}
            CONFIGURATIONS RelWithDebInfo
            COMPONENT Test)
          INSTALL(FILES $<TARGET_FILE_DIR:${target}>/${target}${debug_suffix}.pdb
            DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN}
            CONFIGURATIONS Debug
            COMPONENT Test)
        else()
          INSTALL(FILES $<TARGET_FILE_DIR:${target}>/${target}.pdb
            DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN}
            CONFIGURATIONS RelWithDebInfo
            COMPONENT Development)
          INSTALL(FILES $<TARGET_FILE_DIR:${target}>/${target}${debug_suffix}.pdb
            DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN}
            CONFIGURATIONS Debug
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

#We need to collect all library paths so that tests on windows (where there is no rpath equivalent)
#and java and dotnet tests on both platforms can find libraries.
#we also collect paths to make header check test linking work
if (MSVC)
  set_property(GLOBAL PROPERTY SAFIR_LIBRARY_PATH "$ENV{PATH}")
else()
  set_property(GLOBAL PROPERTY SAFIR_LIBRARY_PATH "$ENV{LD_LIBRARY_PATH}")
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
  if(NOT is_imported EQUAL -1)
    #call the real add_library function
    _add_library(${NAME} ${ARGN})

    #and get out of here, the rest of this function is only for our own targets
    return()
  endif()

  if (CMAKE_VERSION VERSION_LESS "3.0.0")
    if (WIN32)
      file(TO_NATIVE_PATH "${CMAKE_CURRENT_BINARY_DIR}" dir_to_add)
    else()
      set(dir_to_add "${CMAKE_CURRENT_BINARY_DIR}")
    endif()
  else()
    set (dir_to_add "$<TARGET_FILE_DIR:${NAME}>")
  endif()
  get_property(SAFIR_LIBRARY_PATH GLOBAL PROPERTY SAFIR_LIBRARY_PATH)

  if (NOT SAFIR_LIBRARY_PATH)
    set_property(GLOBAL PROPERTY SAFIR_LIBRARY_PATH "${dir_to_add}")
  else()
    set_property(GLOBAL PROPERTY SAFIR_LIBRARY_PATH "${dir_to_add}${PATH_SEPARATOR}${SAFIR_LIBRARY_PATH}")
  endif()

  if (WIN32)
    #This will not work for studio builds, so header_check for studio generator should be skipped.
    get_property(SAFIR_HEADER_CHECK_LINK_PATH GLOBAL PROPERTY SAFIR_HEADER_CHECK_LINK_PATH)
    set_property(GLOBAL PROPERTY SAFIR_HEADER_CHECK_LINK_PATH ${SAFIR_HEADER_CHECK_LINK_PATH}
        "${CMAKE_CURRENT_BINARY_DIR}")
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
#          If this option is not specified the timeout will be set to 60s by default.
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
    SET_TESTS_PROPERTIES(${PROP_TEST} PROPERTIES TIMEOUT 60)
  endif()

  if (NOT PROP_CONFIG_OVERRIDE)
    set(PROP_CONFIG_OVERRIDE "${safir-sdk-core_SOURCE_DIR}/src/tests/test_support/test_config")
  endif()

  set (pypath "$ENV{PYTHONPATH}${PATH_SEPARATOR}${safir-sdk-core_SOURCE_DIR}/src/tests/test_support/python")
  string(REGEX REPLACE "^${PATH_SEPARATOR}+" "" pypath ${pypath}) # remove any leading path separators

  #Add the library path to the environment
  get_property(SAFIR_LIBRARY_PATH GLOBAL PROPERTY SAFIR_LIBRARY_PATH)
  if (MSVC)
    STRING(REPLACE "\;" ";" SAFIR_LIBRARY_PATH "${SAFIR_LIBRARY_PATH}")
    STRING(REPLACE ";" "\\;" SAFIR_LIBRARY_PATH "${SAFIR_LIBRARY_PATH}")
    set(PATH_ENVIRONMENT "PATH=${SAFIR_LIBRARY_PATH}")
  else()
    set(PATH_ENVIRONMENT "LD_LIBRARY_PATH=${SAFIR_LIBRARY_PATH}")
  endif()

  SET_PROPERTY(TEST ${PROP_TEST}
    PROPERTY ENVIRONMENT
    "SAFIR_TEST_CONFIG_OVERRIDE=${PROP_CONFIG_OVERRIDE}"
    "SAFIR_SOURCE_ROOT=${safir-sdk-core_SOURCE_DIR}/"
    "CMAKE_CURRENT_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}"
    "CMAKE_CURRENT_BINARY_DIR=${CMAKE_CURRENT_BINARY_DIR}"
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
FIND_PACKAGE(PythonInterp)
if(PYTHON_VERSION_STRING VERSION_LESS "2.7")
  message(FATAL_ERROR "Need Python version 2.7 or greater. Sorry.")
endif()

#
# Set up Protobuf
#
if (WIN32)
  #Try to make Safir able to find Protobuf on windows
  if (NOT "$ENV{PROTOBUF_DIR}" STREQUAL "")
    set (PROTOBUF_SRC_ROOT_FOLDER $ENV{PROTOBUF_DIR})
  endif()
endif()

FIND_PACKAGE(Protobuf REQUIRED)
INCLUDE_DIRECTORIES(${PROTOBUF_INCLUDE_DIRS})

#
# At last, we're ready to add our subdirectories.
#

add_subdirectory(patch_check)
add_subdirectory(lluf)
add_subdirectory(logging)
add_subdirectory(distribution)
#add_subdirectory(dots)
#add_subdirectory(safir_dou)
#add_subdirectory(control)
#add_subdirectory(dose)
#add_subdirectory(douf)
#add_subdirectory(swre)
#add_subdirectory(foreach)
#add_subdirectory(olib)
#add_subdirectory(dope)

#add_subdirectory(tests)


add_subdirectory(util)
add_subdirectory(config)

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

INSTALL(FILES
  cmake/UseJavaWithManifest/README.txt
  cmake/UseJavaWithManifest/Copyright.txt
  cmake/UseJavaWithManifest/CMakeParseArguments.cmake
  cmake/UseJavaWithManifest/UseJava.cmake
  cmake/UseJavaWithManifest/UseJavaSymlinks.cmake
  cmake/UseJavaWithManifest/UseJavaClassFilelist.cmake
  DESTINATION ${SAFIR_INSTALL_DESTINATION_CMAKE}/UseJavaWithManifest
  COMPONENT Development)
