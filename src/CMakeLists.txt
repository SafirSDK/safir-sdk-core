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


# MACRO(INSTALL_DEBUG_INFO target)
#   if(MSVC)
#     #the problem here is to find out where the pdb file is located. It is located next to the binary
#     #in some directory which either the nmake/jom builds create or that the studio creates.

#     GET_TARGET_PROPERTY(debug_location ${target} LOCATION_Debug)
#     GET_TARGET_PROPERTY(relwithdebinfo_location ${target} LOCATION_RelWithDebInfo)

#     #replace binary's the extension with .pdb
#     #.exe --> .pdb
#     #.dll --> .pdb
#     STRING(REPLACE .dll .pdb debug_location ${debug_location})
#     STRING(REPLACE .exe .pdb debug_location ${debug_location})

#     STRING(REPLACE .dll .pdb relwithdebinfo_location ${relwithdebinfo_location})
#     STRING(REPLACE .exe .pdb relwithdebinfo_location ${relwithdebinfo_location})
    
#     #TODO
#     #Install the pdb files using the locations we just worked out.
#     #INSTALL(FILES ${debug_location} DESTINATION ${SAFIR_RUNTIME}/bin CONFIGURATIONS Debug)
#     #INSTALL(FILES ${relwithdebinfo_location} DESTINATION ${SAFIR_RUNTIME}/bin CONFIGURATIONS RelWithDebInfo)
    
#     UNSET(debug_location)
#     UNSET(relwithdebinfo_location)
#   endif()
# ENDMACRO()

function (SAFIR_INSTALL) 
  cmake_parse_arguments(INST "" "" "TARGETS" ${ARGN})

  if (NOT "${INST_UNPARSED_ARGUMENTS}" STREQUAL "")
    message(FATAL_ERROR "Unknown argument to SAFIR_INSTALL '${INST_UNPARSED_ARGUMENTS}'")
  endif()

    
  if (INST_TARGETS)
    foreach (target ${INST_TARGETS})
      get_target_property(target_type ${target} TYPE)
      if ("${target_type}" STREQUAL "SHARED_LIBRARY")
        set(export EXPORT SafirSDKCore)
      endif()

      INSTALL(TARGETS ${target}
        ${export}
        RUNTIME DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN}
        LIBRARY DESTINATION ${SAFIR_INSTALL_DESTINATION_LIB}
        ARCHIVE DESTINATION ${SAFIR_INSTALL_DESTINATION_LIB})
    endforeach()
  endif()

  #TODO debug info
endfunction()


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
    #TODO this will not work for studio builds, so disable header check for studio generator...
    get_property(SAFIR_HEADER_CHECK_LINK_PATH GLOBAL PROPERTY SAFIR_HEADER_CHECK_LINK_PATH)
    set_property(GLOBAL PROPERTY SAFIR_HEADER_CHECK_LINK_PATH ${SAFIR_HEADER_CHECK_LINK_PATH} 
        "${CMAKE_CURRENT_BINARY_DIR}")
  endif()
  
  #call the real add_library function
  _add_library(${NAME} ${ARGN})
endfunction()

# Set up properties on tests
# Usage: SET_SAFIR_TEST_PROPERTIES(TEST <test_target> [TIMEOUT <timeout>] [CONFIG_OVERRIDE <config_dir>] [ENVIRONMENT ...])
#
# TIMEOUT (optional): Time in seconds after which the test is interrupted. If not specified will be set to 60s.
# CONFIG_OVERRIDE (optional): Absolute path to directory containing safir configuration. If not specified
#                             it will be set to a default configuration.
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
    set(PROP_CONFIG_OVERRIDE "${safir_sdk_core_SOURCE_DIR}/src/tests/test_support/test_config")
  endif()

  set (pypath "$ENV{PYTHONPATH}${PATH_SEPARATOR}${safir_sdk_core_SOURCE_DIR}/src/tests/test_support/python")
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
    "SAFIR_SOURCE_ROOT=${safir_sdk_core_SOURCE_DIR}/"
    "CMAKE_CURRENT_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}"
    "CMAKE_CURRENT_BINARY_DIR=${CMAKE_CURRENT_BINARY_DIR}"
    "PYTHONPATH=${pypath}"
    "${PATH_ENVIRONMENT}"
    ${PROP_ENVIRONMENT})
endfunction()

INCLUDE(build_generated)

include_directories(lluf/lluf_build_helpers.ss/src)

ADD_DEFINITIONS(-DSAFIR_NO_AUTOLINK)


#TODO: make this setting available from ccmake et al
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
# At last, we're ready to add our subdirectories.
#

add_subdirectory(patch_check)
add_subdirectory(lluf)
add_subdirectory(logging)
add_subdirectory(dots)
add_subdirectory(safir_dou)
add_subdirectory(dose)
add_subdirectory(douf)
add_subdirectory(swre)
add_subdirectory(foreach)
add_subdirectory(olib)
add_subdirectory(dope)

add_subdirectory(tests)


message("CONFIG: ${CMAKE_INSTALL_PREFIX}/${SAFIR_INSTALL_DESTINATION_DOU_BASE}")

configure_file(config/typesystem.ini.in
  "${CMAKE_CURRENT_BINARY_DIR}/config/typesystem.ini" @ONLY)

INSTALL(FILES 
  "${CMAKE_CURRENT_BINARY_DIR}/config/typesystem.ini"
  config/locations.ini
  config/logging.ini
  DESTINATION ${SAFIR_INSTALL_DESTINATION_DOC}/example_configuration)

INSTALL(EXPORT SafirSDKCore DESTINATION ${SAFIR_INSTALL_DESTINATION_CMAKE})

configure_file(cmake/SafirSDKCoreConfig.cmake.in
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SafirSDKCoreConfig.cmake" @ONLY)

INSTALL(FILES 
  cmake/build_generated.cmake 
  cmake/CSharpMacros.cmake
  cmake/SafirCompilerSettings.cmake
  cmake/SafirLoadCSharp.cmake
  cmake/FindCSharp.cmake
  cmake/SafirLoadJava.cmake
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SafirSDKCoreConfig.cmake"
  DESTINATION ${SAFIR_INSTALL_DESTINATION_CMAKE})



#TODO: install users guide in doc?
#TODO: install requirements in doc?
#TODO: install examples in doc

