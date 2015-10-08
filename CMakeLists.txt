PROJECT(safir-sdk-core CXX C)

#We need a very recent cmake on windows. The reason is that test property
#and install generator expressions are only supported in cmake 3.
if(MSVC)
  cmake_minimum_required(VERSION 3.0.0)
else()
  cmake_minimum_required(VERSION 2.8.12)
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
