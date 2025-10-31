cmake_minimum_required(VERSION 3.24.0)

#Don't add /W3 to MSVC compilation flags. We set warning levels ourselves
cmake_policy(SET CMP0092 NEW)

#This allows the build script to specify which vs toolset to use. That is currently
#the only thing this is used for.
if (DEFINED ENV{CONAN_EXTRA_ARGUMENTS})
  separate_arguments(CONAN_EXTRA_ARGUMENTS UNIX_COMMAND $ENV{CONAN_EXTRA_ARGUMENTS})
  set(CONAN_INSTALL_ARGS --build=missing ${CONAN_EXTRA_ARGUMENTS} CACHE STRING "Command line arguments for conan install" FORCE)
endif()

set (CMAKE_PROJECT_TOP_LEVEL_INCLUDES ${CMAKE_SOURCE_DIR}/src/cmake/conan_provider.cmake)
PROJECT(safir-sdk-core CXX C)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_CSHARP_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/safir-sdk-core)
set(CMAKE_JAVA_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/share/java/safir-sdk-core)

foreach(_config_type IN LISTS CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_${_config_type} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${_config_type})
endforeach()

file(MAKE_DIRECTORY ${CMAKE_CSHARP_OUTPUT_DIRECTORY})

find_program(CCACHE_FOUND ccache)
if(CCACHE_FOUND)
  message(STATUS "Found ccache, using it to speed up compilation.")
  set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
endif(CCACHE_FOUND)

# Define RUN_CLANG_TIDY on the command line if you want to run clangs static analysis tool.
# With these settings there are only a few warnings given. Other settings yield more...
if (RUN_CLANG_TIDY)
  set(CMAKE_CXX_CLANG_TIDY
    clang-tidy;
    -header-filter=.;
    -checks=-*,clang-analyzer-*;
  )
endif()

#Set up the Safir SDK Core version by parsing version.txt
#IMPORTANT: THE version.txt FILE NEEDS TO BE UPDATED FOR EVERY RELEASE
#Remember: any updates that change APIs should update MAJOR, since
#that is used for SOVERSION.
#Set the SUFFIX to empty string for stable releases
file(STRINGS VERSION.txt version_strings)
foreach (line IN LISTS version_strings)
  #skip comments and empty lines
  if (NOT line MATCHES "^#.*" AND NOT line STREQUAL "")
    string(REPLACE "=" ";" line ${line})
    list(GET line 0 key)
    list(GET line 1 value)
    if (key STREQUAL "MAJOR")
      set(SAFIR_VERSION_MAJOR "${value}")
    elseif(key STREQUAL "MINOR")
      set(SAFIR_VERSION_MINOR "${value}")
    elseif(key STREQUAL "PATCH")
      set(SAFIR_VERSION_PATCH "${value}")
    elseif(key STREQUAL "SUFFIX")
      set(SAFIR_VERSION_SUFFIX "${value}")
    else()
      message(FATAL_ERROR "Failed to parse version.txt file")
    endif()
  endif()
endforeach()

SET(SAFIR_VERSION_STRING "${SAFIR_VERSION_MAJOR}.${SAFIR_VERSION_MINOR}.${SAFIR_VERSION_PATCH}${SAFIR_VERSION_SUFFIX}")

#make the version available to code as well.
ADD_DEFINITIONS(-DSAFIR_SDK_CORE_VERSION=\"${SAFIR_VERSION_STRING}\")

#
# This function will install a python script into the bin directory.
# If it is a *nix system the script will have ".py" stripped from its name.
#
# Usage: SAFIR_INSTALL_PYTHON(SCRIPT <script>
#                             [RENAME <new name>]
#                             [COMPONENT <component>])
#
# RENAME can be used to rename a python script during installation. Please
#        include the .py extension in the argument, it will still be stripped
#        during installation.
# COMPONENT can be used to specify which component the script belongs to.
#
function (SAFIR_INSTALL_PYTHON)
  cmake_parse_arguments(INST "" "SCRIPT;COMPONENT;RENAME" "" ${ARGN})

  if (NOT "${INST_UNPARSED_ARGUMENTS}" STREQUAL "")
    message(FATAL_ERROR "Unknown argument to SAFIR_INSTALL_PYTHON '${INST_UNPARSED_ARGUMENTS}'")
  endif()

  if(INST_RENAME AND NOT MSVC)
    STRING(REGEX REPLACE "\\.py$" "" _stripped_name "${INST_RENAME}")
    GET_FILENAME_COMPONENT(_stripped_name ${_stripped_name} NAME)
    SET(_RENAME_ARG RENAME ${_stripped_name})
  elseif(INST_RENAME)
    SET(_RENAME_ARG RENAME ${INST_RENAME})
  elseif(NOT MSVC)
    STRING(REGEX REPLACE "\\.py$" "" _stripped_name "${INST_SCRIPT}")
    GET_FILENAME_COMPONENT(_stripped_name ${_stripped_name} NAME)
    SET(_RENAME_ARG RENAME ${_stripped_name})
  endif()

  INSTALL(PROGRAMS ${INST_SCRIPT}
    DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN}
    COMPONENT ${INST_COMPONENT}
    ${_RENAME_ARG})
endfunction()

MESSAGE(STATUS "CMAKE_SIZEOF_VOID_P = ${CMAKE_SIZEOF_VOID_P}")
MESSAGE(STATUS "CMAKE_INSTALL_PREFIX = ${CMAKE_INSTALL_PREFIX}")

if (WIN32 AND
    CMAKE_SIZEOF_VOID_P MATCHES "8" AND
    CMAKE_INSTALL_PREFIX MATCHES "Program Files \\(x86\\)")
  MESSAGE(STATUS "Incorrectly set CMAKE_INSTALL_PREFIX. Contains 'Program Files (x86)' even though we're building "
    "in 64 bit mode (according to CMAKE_SIZEOF_VOID_P). Will remove ' (x86)' from CMAKE_INSTALL_PREFIX.")
  STRING(REPLACE " (x86)" "" CMAKE_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX})
endif()

set(CMAKE_INSTALL_MESSAGE LAZY)

#set up install directories
SET(SAFIR_INSTALL_DESTINATION_INCLUDE include)
SET(SAFIR_INSTALL_DESTINATION_BIN bin)
SET(SAFIR_INSTALL_DESTINATION_LIB lib)

if (UNIX)
  SET(SAFIR_INSTALL_DESTINATION_CMAKE lib/cmake/SafirSDKCore)
  SET(SAFIR_INSTALL_DESTINATION_DOC share/doc/safir-sdk-core)
  SET(SAFIR_INSTALL_DESTINATION_MAN share/man)
  SET(SAFIR_INSTALL_DESTINATION_CSHARP lib/safir-sdk-core)
  SET(SAFIR_INSTALL_DESTINATION_JAR share/java/safir-sdk-core)
  SET(SAFIR_INSTALL_DESTINATION_TEST_DATA share/safir-sdk-core/test_data)
  SET(SAFIR_INSTALL_DESTINATION_GENERATION share/safir-sdk-core/generation)
  SET(SAFIR_INSTALL_DESTINATION_DB share/safir-sdk-core/db)
  SET(SAFIR_INSTALL_DESTINATION_DOU_BASE share/safir-sdk-core/dou)
else()
  SET(SAFIR_INSTALL_DESTINATION_CMAKE cmake)
  SET(SAFIR_INSTALL_DESTINATION_DOC docs)
  SET(SAFIR_INSTALL_DESTINATION_CSHARP dotnet)
  SET(SAFIR_INSTALL_DESTINATION_JAR java)
  SET(SAFIR_INSTALL_DESTINATION_TEST_DATA test_data)
  SET(SAFIR_INSTALL_DESTINATION_GENERATION generation)
  SET(SAFIR_INSTALL_DESTINATION_DB db)
  SET(SAFIR_INSTALL_DESTINATION_DOU_BASE dou)
  SET(SAFIR_INSTALL_DESTINATION_INST_UTIL installer_utils)
  set(CMAKE_DEBUG_POSTFIX d)
endif()

SET(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/src/cmake ${CMAKE_MODULE_PATH})

INCLUDE_DIRECTORIES(include)
INSTALL(DIRECTORY include/ DESTINATION ${SAFIR_INSTALL_DESTINATION_INCLUDE}
  COMPONENT Development
  PATTERN ".svn" EXCLUDE
  PATTERN "*~" EXCLUDE)

#This makes the include files show up in various IDEs
FILE(GLOB_RECURSE include_headers include/*.h)
ADD_CUSTOM_TARGET(dummy_header_target SOURCES ${include_headers})

#Note: do not try to use this file from within the Safir SDK Core build tree!
#use the version defined using ADD_DEFINITIONS above instead.
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/SafirSdkCoreVersion.h
  "//This is an automatically generated file.\n"
  "#pragma once\n"
  "#define SAFIR_SDK_CORE_VERSION_MAJOR ${SAFIR_VERSION_MAJOR}\n"
  "#define SAFIR_SDK_CORE_VERSION_MINOR ${SAFIR_VERSION_MINOR}\n"
  "#define SAFIR_SDK_CORE_VERSION_PATCH ${SAFIR_VERSION_PATCH}\n"
  "#define SAFIR_SDK_CORE_VERSION_SUFFIX \"${SAFIR_VERSION_SUFFIX}\"\n"
  "#define SAFIR_SDK_CORE_VERSION_STRING \"${SAFIR_VERSION_STRING}\"\n"
  )

INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/SafirSdkCoreVersion.h
  DESTINATION ${SAFIR_INSTALL_DESTINATION_INCLUDE}/Safir
  COMPONENT Development)


set(CTEST_FULL_OUTPUT 1)
ENABLE_TESTING()
configure_file(CTestCustom.cmake . COPYONLY)
set_property (GLOBAL PROPERTY USE_FOLDERS True)

add_subdirectory(src)
add_subdirectory(docs)

SAFIR_INSTALL_PYTHON(
  SCRIPT build/build.py
  RENAME dobmake-batch.py
  COMPONENT Development)

INSTALL(FILES COPYING.txt
  DESTINATION ${SAFIR_INSTALL_DESTINATION_DOC}
  RENAME LICENSE.txt
  COMPONENT Runtime)

INSTALL(FILES build/packaging/debian/copyright
  DESTINATION ${SAFIR_INSTALL_DESTINATION_DOC}
  RENAME third_party_licenses.txt
  COMPONENT Runtime)

INSTALL(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/conan/licenses/
  DESTINATION ${SAFIR_INSTALL_DESTINATION_DOC}/more_third_party_licenses
  COMPONENT Runtime
  PATTERN .gitignore EXCLUDE)

if (WIN32)
  INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/conan/bin/ninja.exe
    DESTINATION ${SAFIR_INSTALL_DESTINATION_BIN}
    COMPONENT Development)
endif()

set(ignore_me
  "Some of our build methods pass along extra variables to cmake, which causes warnings."
  "Add them below to get rid of the warning."
  ${CMAKE_EXPORT_NO_PACKAGE_REGISTRY}
  ${CMAKE_FIND_PACKAGE_NO_PACKAGE_REGISTRY}
  ${CMAKE_INSTALL_LIBDIR}
  ${CMAKE_INSTALL_LOCALSTATEDIR}
  ${CMAKE_INSTALL_SYSCONFDIR}
  ${CMAKE_AUTOGEN_VERBOSE}
  ${FETCHCONTENT_FULLY_DISCONNECTED}
  )


